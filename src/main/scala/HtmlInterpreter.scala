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
    val updateBody = new StringBuilder()
    val doc = Jsoup.parse(html)
    val root = doc.child(0)
    def visitNode(node: Node, depth: Int, seq: Int) {
      node match {
        case elem: Element 
        if elem.tagName == "script" && elem.attr("type") == "shtml" => {
          elem.data.split("\n") foreach { line =>
            val matched = tokenizeSelectors.matcher(line)
            if (matched.matches) {
              val name = matched.group(2)
              updateTypes.append("  type ")
              updateTypes.append(name)
              updateTypes.append(" = ")
              updateTypes.append(className)
              updateTypes.append(buildPath(doc.select(matched.group(1)).first()).mkString(".", ".", ""))
              updateTypes.append(".type\n")
              updateParams ::= "update" + name + ": " + name + " -> Node"
            }
          }
        }
        case elem: Element => {
          indent(depth, result)
          elemStart("_" + seq, elem.tagName, elem.attributes, depth, result)
          elem.childNodes.asScala.zipWithIndex foreach { case (node, seq) =>
            visitNode(node, depth + 1, seq)
          }
          indent(depth, result)
          result.append("}\n")
        }
        case text: TextNode => {
          indent(depth, result)
          obj(seq, result)
          result.append(" extends Text {\n")
          indent(depth + 1, result)
          prop("text", text.text, result)
          indent(depth, result)
          result.append("}\n")
        }
      }
    }

    result.append("package ")
    result.append(pkg.mkString("."))
    result.append("\n")
    
    elemStart(className, root.tagName, root.attributes, 0, result)
    root.childNodes.asScala.zipWithIndex foreach { case (node, seq) =>
      visitNode(node, 1, seq)
    }
    if (updateParams.size > 0) {
      result.append(updateTypes)
      result.append("  def update(")
      result.append(updateParams.reverse.mkString(", "))
      result.append("): String = {\n")
      result.append(updateBody)
      result.append("  }\n")
    }
    result.append("}\n")
    result.toString
  }
}
