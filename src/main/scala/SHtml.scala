package com.github.dmlap.shtml

import scala.collection.JavaConverters._
import java.util.regex.Pattern
import org.jsoup.Jsoup
import org.jsoup.nodes.{Attributes, DataNode, Element, Node, TextNode}

object SHtml extends Application {

  @inline private def indent(level: Int, builder: StringBuilder) {
    (0 until level) foreach { _ => builder.append("  ") }
  }
  @inline private def obj(seq: Int, builder: StringBuilder) {
    builder.append("object _")
    builder.append(seq)
  }
  @inline private def prop(name: String, value: String, builder: StringBuilder) {
    builder.append("val " + name + " = \"")
    builder.append(value)
    builder.append("\"\n")
  }
  private def buildPath(elem: Element, path: List[String] = Nil): List[String] =
    if (elem.parent == elem.ownerDocument) {
      path
    } else {
      buildPath(elem.parent, "_" + elem.siblingIndex :: path)
    }

  val tokenizeSelectors = Pattern.compile("""\s*([^\s]+)\s+->\s+(\w+)\s*""")

  private trait NodeVisitor {
    def onText(text: TextNode, depth: Int, seq: Int)
    def onElemStart(elem: Element, depth: Int, seq: Int)
    def onElemEnd(elem: Element, depth: Int, seq: Int)
  }
  private def traverse(node: Node, depth: Int = 0, seq: Int = 0, visitor: NodeVisitor) {
    node match {
      case elem: Element => {
        visitor.onElemStart(elem, depth, seq)
        elem.childNodes.iterator.asScala.zipWithIndex foreach { case (node, seq) =>
          traverse(node, depth + 1, seq, visitor)
        }
        visitor.onElemEnd(elem, depth, seq)
      }
      case text: TextNode => {
        visitor.onText(text, depth, seq)
      }
    }
  }
  
  def parse(pkg: List[String],
            className: String,
            html: String): String = {
    val result = new StringBuilder()
    val updateTypes = new StringBuilder()
    var updateParams: List[String] = Nil
    val doc = Jsoup.parse(html)
    val root = doc.child(0)
    val updateBody = new StringBuilder("    \"\"\"<!doctype html>")
    var snippets = Map.empty[Element, (String, String)]

    // Find all snippets, record their contents and then remove them
    root.select("script[type=shtml]").iterator.asScala foreach { element =>
      element.data.split("\n") foreach { line =>
        val matched = tokenizeSelectors.matcher(line)
        if (matched.matches) {
          val name = matched.group(2)
          val elem = doc.select(matched.group(1)).first()
          val path = className.capitalize +
            buildPath(elem).mkString(".", ".", "")
          updateTypes.append("  type " + name + " = " + path + ".type\n")
          updateParams ::= "update" + name + ": " + name + " => Node"
          snippets += elem -> (name -> path)
        }
      }
      element.remove()
    }
    
    if (pkg.size > 0) {
      result.append("package ")
      result.append(pkg.mkString("."))
      result.append("\n")
    }
    result.append("import com.github.dmlap.{Elem, Text}\n")
    
    // build the update function
    traverse(node = root, visitor = new NodeVisitor {
      var pruneToDepth: Int = Integer.MAX_VALUE
      def onText(text: TextNode, depth: Int, seq: Int) {
        if (depth < pruneToDepth) {
          updateBody.append(text.text)
        }
      }
      def onElemStart(elem: Element, depth: Int, seq: Int) {
        (pruneToDepth, snippets.get(elem)) match {
          case (pruneDepth, _) if depth >= pruneDepth => ()
          case (_, Some((name, path))) => {
            updateBody.append("\"\"\" + update" + name + "(" + path + ").asString + \"\"\"")
            pruneToDepth = depth
          }
          case _ => {
            updateBody.append("<" + elem.tagName)
            if (elem.attributes.size > 0) {
              updateBody.append(elem.attributes)
            }
            updateBody.append(">")
          }
        }
      }
      def onElemEnd(elem: Element, depth: Int, seq: Int) {
        (pruneToDepth, snippets.get(elem)) match {
          case (pruneDepth, _) if depth == pruneDepth =>
            pruneToDepth = Integer.MAX_VALUE
          case (pruneDepth, _) if depth > pruneDepth => ()
          case _ => updateBody.append("</" + elem.tagName + ">")
        }
      }
    })

    // output Scala objects corresponding to DOM nodes
    traverse(node = root, visitor = new NodeVisitor {
      def onText(text: TextNode, depth: Int, seq: Int) {
        indent(depth, result)
        obj(seq, result)
        result.append(" extends Text {\n")
        indent(depth + 1, result)
        prop("text", text.text, result)
        indent(depth, result)
        result.append("}\n")
      }
      def onElemStart(elem: Element, depth: Int, seq: Int) {
        indent(depth, result)
        result.append("object ")
        val name = if (depth == 0) {
          className.capitalize
        } else {
          "_" + seq
        }
        result.append(name)
        result.append(" extends Elem {\n")
        indent(depth + 1, result)
        prop("name", elem.tagName, result)
        elem.attributes.iterator.asScala foreach { attr =>
          indent(depth + 1, result)
          prop("`" + attr.getKey + "`", attr.getValue, result)
        }
      }
      def onElemEnd(elem: Element, depth: Int, seq: Int) {
        if (elem.childNodes.size > 0) {
          indent(depth + 1, result)
          result.append("override val children = List(")
          result.append((0 until elem.childNodes.size).map("_" + _).mkString(", "))
          result.append(")\n")
        }
        if (depth == 0 && updateParams.size > 0) {
          result.append(updateTypes)
          result.append("  def update(")
          result.append(updateParams.reverse.mkString(", "))
          result.append("): String = \n")
          result.append(updateBody)
          result.append("\"\"\"\n")
        }
        indent(depth, result)
        result.append("}\n")
      }
    })
    result.toString
  }
}
