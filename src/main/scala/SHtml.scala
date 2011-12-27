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

    def elemStart(className: String,
                  elem: Element,
                  level: Int) {
      result.append("object ")
      result.append(className)
      result.append(" extends Elem {\n")
      indent(level + 1, result)
      prop("name", elem.tagName, result)
      elem.attributes.iterator.asScala foreach { attr =>
        indent(level + 1, result)
        prop("`" + attr.getKey + "`", attr.getValue, result)
      }
      val outputNodeCount = elem.childNodes.asScala.foldLeft(0) { (seq, node) =>
        if (visitNode(node, level + 1, seq)) {
          seq + 1
        } else {
          seq
        }
      }
      if (outputNodeCount > 0) {
        indent(level + 1, result)
        result.append("override val children = List(")
        result.append((0 until outputNodeCount).map("_" + _).mkString(", "))
        result.append(")\n")
      }
    }

    def visitNode(node: Node, depth: Int, seq: Int): Boolean = {
      node match {
        case elem: Element => {
          snippets.get(elem) match {
            case Some((name, path)) => {
              updateBody.append("\"\"\" + update" + name + "(" + path + ").asString + \"\"\"")
              indent(depth, result)
              elemStart("_" + seq, elem, depth)
            }
            case _ => {
              updateBody.append("<" + elem.tagName)
              if (elem.attributes.size > 0) {
                updateBody.append(elem.attributes)
              }
              updateBody.append(">")
              indent(depth, result)
              elemStart("_" + seq, elem, depth)
              updateBody.append("</" + elem.tagName + ">")
            }
          }
          indent(depth, result)
          result.append("}\n")
          true
        }
        case text: TextNode => {
          indent(depth, result)
          obj(seq, result)
          result.append(" extends Text {\n")
          indent(depth + 1, result)
          prop("text", text.text, result)
          indent(depth, result)
          result.append("}\n")
          updateBody.append(text.text)
          true
        }
      }
    }
    
    if (pkg.size > 0) {
      result.append("package ")
      result.append(pkg.mkString("."))
      result.append("\n")
    }
    result.append("import com.github.dmlap.{Elem, Text}\n")
    
    
    updateBody.append("<" + root.tagName)
    if (root.attributes.size > 0) {
      updateBody.append(root.attributes)
    }
    updateBody.append(">")
    elemStart(className.capitalize, root, 0)
    if (updateParams.size > 0) {
      result.append(updateTypes)
      result.append("  def update(")
      result.append(updateParams.reverse.mkString(", "))
      result.append("): String = \n")
      result.append(updateBody)
      result.append("</" + root.tagName + ">\"\"\"\n")
    }
    result.append("}\n")
    result.toString
  }
}
