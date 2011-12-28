package com.github.dmlap.shtml

import org.specs2.mutable._

object SHtmlSpecs extends Specification {
  "SHtml" should {
    "correctly interpret a valid, minimal HTML document:" in {
      val actualTrivial = parseHtmlWithBody("")
      val expectedTrivial =
      "package com.example\n" +
      "import com.github.dmlap.{Elem, Text}\n" +
      "object Example extends Elem {\n" +
      "  val name = \"html\"\n" +
      "  object _0 extends Elem {\n" +
      "    val name = \"head\"\n" +
      "    object _0 extends Elem {\n" +
      "      val name = \"title\"\n" +
      "    }\n" +
      "    override val children = List(_0)\n" +
      "  }\n" +
      "  object _1 extends Elem {\n" +
      "    val name = \"body\"\n" +
      "  }\n" +
      "  override val children = List(_0, _1)\n" +
      "}\n"
      actualTrivial must_== expectedTrivial
    }
    "create properties for child elements of the body" in {
      val actualPs = parseHtmlWithBody("<p /><p /><p />")
      val hasPs = java.util.regex.Pattern.compile(".*(.+val name = \"p\"){3}.*", java.util.regex.Pattern.DOTALL)
      true must_== hasPs.matcher(actualPs).matches
    }
    "create text nodes" in {
      val result = parseHtmlWithBody("abc")
      result must contain("val text = \"abc\"")
      result must contain(" extends Text {\n")
    }
    "record attributes" in {
      parseHtmlWithBody("<div class='c0 c1' />") must contain("val `class` = \"c0 c1\"")
    }
    "parse CSS selector mappings" in {
      val script =
        "<div id='id' />" +
        "<script type='shtml'>\n" +
        "  #id -> Id  \n" +
        "</script>\n"
      val parsed = parseHtmlWithBody(script)
      parsed must contain("type Id = Example._1._0.type")
      parsed must contain("def update(updateId: Id => Node): String = \n")
      parsed must contain("<!doctype html><html>")
      parsed must contain("updateId(Example._1._0).asString")
    }
    "not emit a package declaration if none is provided" in {
      SHtml.parse(Nil, "Example", "") must not contain("package")
    }
    "capitalize the top-level object name" in {
      val result = SHtml.parse(Nil, "example", "<div id='id' /><script type='shtml'>#id -> Id</script>")
      result must contain("object Example")
      result must contain("type Id = Example.")
      result must contain("updateId(Example.")
    }
    "filter out SHTML script nodes in the output" in {
      parseHtmlWithBody("<p></p><script type='shtml'></script><p></p>") must not contain("List(_0, _1, _2)")
    }
    "invoke snippets at the correct location in the output of the update function " in {
      val result = parseHtmlWithBody("<div id='id'></div><div></div><script type='shtml'>#id -> Id</script>")
      result must not be matching("<div id=['\"]id['\"]")
      result must contain("</title></head><body>\"\"\" + updateId(Example._1._0).asString + \"\"\"<div></div>")
    }
    "not output child nodes of snippet elements unless they were included in the snippet output" in {
      val result = parseHtmlWithBody("<div id='id'><p>hi</div><script type='shtml'>#id -> Id</script>")
      result must not contain("<div")
      result must not contain("<p>")
      result must contain("val name = \"p\"")
      result must not contain("hi</body>")
    } 
  }
  def parseHtmlWithBody(body: String): String = 
    SHtml.parse("com" :: "example" :: Nil,
                          "Example",
                          "<!doctype html><html><head><title></title></head><body>" +
                          body +
                          "</body></html>")
}
