package com.github.dmlap.shtml

import org.specs2.mutable._

object SHtmlSpecs extends Specification {
  "SHtml" should {
    "correctly interpret a valid, minimal HTML document:" in {
      val actualTrivial = parseHtmlWithBody("")
      val expectedTrivial =
      "package com.example\n" +  
      "object Example extends Elem {\n" +
      "  val name = \"html\"\n" +
      "  object _0 extends Elem {\n" +
      "    val name = \"head\"\n" +
      "    object _0 extends Elem {\n" +
      "      val name = \"title\"\n" +
      "    }\n" +
      "  }\n" +
      "  object _1 extends Elem {\n" +
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
    "parses CSS selector mappings" in {
      val script =
        "<div id='id' />" +
        "<script type='shtml'>\n" +
        "#id -> Id\n" +
        "</script>\n"
      val parsed = parseHtmlWithBody(script)
      parsed must contain("type Id = Example._1._0.type")
      parsed must contain("def update(updateId: Id -> Node): String = \n")
      parsed must contain("updateId(Example._1._0)")
    }
  }
  def parseHtmlWithBody(body: String): String = 
    SHtml.parse("com" :: "example" :: Nil,
                          "Example",
                          "<!doctype html><html><head><title></title></head><body>" +
                          body +
                          "</body></html>")
}
