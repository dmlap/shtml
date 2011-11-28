package com.github.dmlap.shtml

import scala.collection.JavaConverters._

sealed trait HtmlEvent
object StartDoc extends HtmlEvent
object EndDoc extends HtmlEvent
final case class StartElem(name: String, attributes: Map[String, String]) extends HtmlEvent
object EndElem extends HtmlEvent
final case class TextNode(text: String) extends HtmlEvent

trait HtmlTokenizer {
  def apply(html: String): Iterator[HtmlEvent]
}

object HtmlInterpreter extends Application {
  implicit object XmlTokenizer extends HtmlTokenizer {
    import javax.xml.stream.XMLInputFactory
    import javax.xml.stream.events.{Attribute, StartDocument, StartElement, EndDocument, EndElement, Characters}
    import java.io.StringReader

    def apply(html: String): Iterator[HtmlEvent] = new Iterator[HtmlEvent] {
      val tokens = XMLInputFactory.newInstance().createXMLEventReader(new StringReader(html))
      def hasNext = tokens.hasNext
      def next() = tokens.next() match {
        case _: StartDocument => StartDoc
        case _: EndDocument => EndDoc
        case startElem: StartElement => {
          val attrs = (Map.empty[String, String] /: startElem.getAttributes.asScala) {
            case (attrs: Map[String, String], attr: Attribute) =>
            attrs + (attr.getName.getLocalPart -> attr.getValue)
          }
          StartElem(startElem.getName.getLocalPart, attrs)
        }
        case _: EndElement => EndElem
        case text: Characters => TextNode(text.getData)
      }
    }
  }

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
                                attrs: Map[String, String],
                                level: Int,
                                builder: StringBuilder) {
    builder.append("object ")
    builder.append(className)
    builder.append(" implements Elem {\n")
    indent(level + 1, builder)
    prop("name", name, builder)
    attrs foreach { attr =>
      indent(level + 1, builder)
      prop("`" + attr._1 + "`", attr._2, builder)
    }
  }

  def parse(pkg: List[String],
            className: String,
            html: String)(implicit tokenizer: HtmlTokenizer): String = {
    val result = new StringBuilder()
    val itr = tokenizer(html)
    var seq = List(0)
    var level = 0
    var foundRoot = false

    result.append("package ")
    result.append(pkg.mkString("."))
    result.append("\n")

    while (!foundRoot && itr.hasNext) {
      itr.next match {
        case StartElem(name, attrs) => {
          elemStart(className, name, attrs, 0, result)
          level += 1
          seq = 0 :: (seq.head + 1) :: seq.tail
          foundRoot = true
        }
        case _ => ()
      }
    }
    while (itr.hasNext) {
      itr.next match {
        case StartElem(name, attrs) => {
          indent(level, result)
          elemStart("_" + seq.head, name, attrs, level, result)
          level += 1
          seq = 0 :: (seq.head + 1) :: seq.tail
        }
        case `EndElem` => {
          indent(level - 1, result)
          level -= 1
          seq = seq.tail
          result.append("}\n")
        }
        case TextNode(text) => {
          indent(level, result)
          obj(seq.head, result)
          result.append(" implements Text {\n")
          indent(level + 1, result)
          prop("text", text, result)
          indent(level, result)
          result.append("}\n")
          seq = (seq.head + 1) :: seq.tail
        }
        case _ => ()
      }
    }
    result.toString
  }
}
