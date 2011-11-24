package com.github.dmlap.shtml

sealed trait HtmlEvent
object StartDoc extends HtmlEvent
object EndDoc extends HtmlEvent
final case class StartElem(name: String) extends HtmlEvent
object EndElem extends HtmlEvent
final case class Text(text: String) extends HtmlEvent

trait HtmlTokenizer {
  def apply(html: String): Iterator[HtmlEvent]
}

trait Elem extends Product {
  val name: String
}

object HtmlInterpreter extends Application {
  implicit object XmlTokenizer extends HtmlTokenizer {
    import javax.xml.stream.XMLInputFactory
    import javax.xml.stream.events.{StartDocument, StartElement, EndDocument, EndElement, Characters}
    import java.io.StringReader
    def apply(html: String): Iterator[HtmlEvent] = new Iterator[HtmlEvent] {
      val tokens = XMLInputFactory.newInstance().createXMLEventReader(new StringReader(html))
      def hasNext() = tokens.hasNext()
      def next() = tokens.next() match {
        case _: StartDocument => StartDoc
        case _: EndDocument => EndDoc
        case startElem: StartElement => StartElem(startElem.getName.getLocalPart)
        case _: EndElement => EndElem
        case text: Characters => Text(text.getData)
      }
    }
  }

  def parse(html: String)(implicit tokenizer: HtmlTokenizer): String = {
    val result = new StringBuilder()
    val itr = tokenizer(html)
    var seq = List(0)
    var level = 0
    while (itr.hasNext) {
      itr.next match {
        case StartElem(name) => {
          val indent = new StringBuilder()
          (0 until level) foreach { _ => indent.append("  ") }
          result.append(indent)
          result.append("object _")
          result.append(seq.head)
          result.append(" implements Elem {\n")
          result.append(indent)
          result.append("  ")
          result.append("val name = \"")
          result.append(name)
          result.append("\"\n")
          level += 1
          seq = 0 :: (seq.head + 1) :: seq.tail
        }
        case `EndElem` => {
          val indent = new StringBuilder()
          (0 until level - 1) foreach { _ => indent.append("  ") }
          result.append(indent)
          level -= 1
          seq = seq.tail
          result.append("}\n")
        }
        case Text(text) => result.append(text)
        case _ => ()
      }
    }
    println("********\n" + result.toString)
    result.toString
  }
  
  // testing
  case class ProductMatcher(product: Product) {
    def matches(rhs: Map[String, Any]) = false
  }

  def wrap(body: String): String = 
    "<html><head><title></title></head><body>" +
      body +
      "</body></html>"

  override def main(args: Array[String]) {
    println("Parse should:")
    println("correctly interpret a valid, minimal HTML document:")
    val actualTrivial = parse(wrap(""))
    val expectedTrivial =
      "object _0 implements Elem {\n" +
      "  val name = \"html\"\n" +
      "  object _0 implements Elem {\n" +
      "    val name = \"head\"\n" +
      "    object _0 implements Elem {\n" +
      "      val name = \"title\"\n" +
      "    }\n" +
      "  }\n" +
      "  object _1 implements Elem {\n" +
      "    val name = \"body\"\n" +
      "  }\n" +
      "}\n"
    println("\t" + (actualTrivial == expectedTrivial))

    println("create properties for child elements of the body")
    val actualPs = parse(wrap("<p /><p /><p />"))
    println("\t" + (actualPs matches ".*(.+val name = \"p\"){3}.*"))
  }
}
