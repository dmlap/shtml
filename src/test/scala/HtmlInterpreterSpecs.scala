package com.github.dmlap.shtml

import org.specs2.mutable._
import HtmlInterpreter.XmlTokenizer

object HtmlInterpreterSpecs extends Specification {
  "HtmlInterpreter" should {
    "correctly interpret a valid, minimal HTML document:" in {
      val actualTrivial = parseHtmlWithBody("")
      val expectedTrivial =
      "package com.example\n" +  
      "object Example implements Elem {\n" +
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
      actualTrivial must_== expectedTrivial
    }
    "create properties for child elements of the body" in {
      val actualPs = parseHtmlWithBody("<p /><p /><p />")
      val hasPs = java.util.regex.Pattern.compile(".*(.+val name = \"p\"){3}.*", java.util.regex.Pattern.DOTALL)
      true must_== hasPs.matcher(actualPs).matches
    }
    "create text nodes" in {
      true must_== (parseHtmlWithBody("abc").indexOf("val text = \"abc\"") >= 0)
    }
    "record attributes" in {
      true must_== (parseHtmlWithBody("<div class='c0 c1' />").indexOf("val `class` = \"c0 c1\"") >= 0)
    }
  }
  def parseHtmlWithBody(body: String): String = 
    HtmlInterpreter.parse("com" :: "example" :: Nil,
                          "Example",
                          "<html><head><title></title></head><body>" +
                          body +
                          "</body></html>")
}
