package com.github.dmlap.shtml

import scala.collection.JavaConverters._
import java.util.regex.Pattern
import org.jsoup.Jsoup
import org.jsoup.nodes.{Attributes, DataNode, Element, Node, TextNode}

object HtmlInterpreter extends Application {

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
  @inline private def elemStart(className: String,
                                name: String,
                                attrs: Attributes,
                                level: Int,
                                builder: StringBuilder) {
    builder.append("object ")
    builder.append(className)
    builder.append(" extends Elem {\n")
    indent(level + 1, builder)
    prop("name", name, builder)
    attrs.iterator.asScala foreach { attr =>
      indent(level + 1, builder)
      prop("`" + attr.getKey + "`", attr.getValue, builder)
    }
  }
  private def buildPath(elem: Element, path: List[String] = Nil): List[String] =
    if (elem.parent == elem.ownerDocument) {
      path
    } else {
      buildPath(elem.parent, "_" + elem.siblingIndex :: path)
    }

  val tokenizeSelectors = Pattern.compile("""([^\s]+)\s+->\s+(\w+)""")
  
  def parse(pkg: List[String],
            className: String,
            html: String): String = {
    val result = new StringBuilder()
    val updateTypes = new StringBuilder()
    var updateParams: List[String] = Nil
    val doc = Jsoup.parse(html)
    val root = doc.child(0)
    val updateBody = new StringBuilder("    \"\"\"<!doctype html><")
    var snippets = Map.empty[String, String]
    def visitNode(node: Node, depth: Int, seq: Int) {
      node match {
        case elem: Element 
        if elem.tagName == "script" && elem.attr("type") == "shtml" => {
          elem.data.split("\n") foreach { line =>
            val matched = tokenizeSelectors.matcher(line)
            if (matched.matches) {
              val name = matched.group(2)
              val path = className +
                buildPath(doc.select(matched.group(1)).first()).mkString(".", ".", "")
              updateBody.append("\"\"\" + update" + name + "(" + path + ") + \"\"\"")
              updateTypes.append("  type " + name + " = " + path + ".type\n")
              updateParams ::= "update" + name + ": " + name + " -> Node"
              snippets += name -> path
            }
          }
        }
        case elem: Element => {
          indent(depth, result)
          elemStart("_" + seq, elem.tagName, elem.attributes, depth, result)
          updateBody.append("<" + elem.tagName)
          if (elem.attributes.size > 0) {
            updateBody.append(elem.attributes)
          }
          updateBody.append(">")
          elem.childNodes.asScala.zipWithIndex foreach { case (node, seq) =>
            visitNode(node, depth + 1, seq)
          }
          indent(depth, result)
          result.append("}\n")
          updateBody.append("</" + elem.tagName + ">")
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
        }
      }
    }

    result.append("package ")
    result.append(pkg.mkString("."))
    result.append("\n")
    
    elemStart(className, root.tagName, root.attributes, 0, result)
    updateBody.append(root.tagName)
    if (root.attributes.size > 0) {
      updateBody.append(" " + root.attributes)
    }
    updateBody.append(">")
    root.childNodes.asScala.zipWithIndex foreach { case (node, seq) =>
      visitNode(node, 1, seq)
    }
    if (updateParams.size > 0) {
      result.append(updateTypes)
      result.append("  def update(")
      result.append(updateParams.reverse.mkString(", "))
      result.append("): String = \n")
      result.append(updateBody)
      result.append("</" + root.tagName + ">\"\"\"\n")
    }
    result.append("}\n")
    println(result)
    result.toString
  }
}
