package com.acrussell.commonmark

import org.scalatest._
import org.scalatest.Matchers._
import scalatags.Text.all._

import com.acrussell.commonmark.CommonMark._

class CommonMarkSuite extends FunSuite {
  test("Horizontal rules should be compiled.") {
    val input = "***\n---\n___"
    assert(Stream(hr(), hr(), hr()).mkString === compileMarkdown(input).mkString)
  }

  test("A single paragraph should be compiled.") {
    val input = "aaa"
    assert(Stream(p("aaa")).mkString === compileMarkdown(input).mkString)
  }

  test("The parsing strategy example should compile correctly.") {
    val input =
      """|> Lorem ipsum dolor
         |sit amet.
         |> - Qui *quodsi iracundia*
         |> - aliquando id""".stripMargin
    val outputHTML =
      Stream(
        blockquote(
          p(raw("Lorem ipsum dolor sit amet.")),
          ul(
            li(p(raw("Qui "), em("quodsi iracundia"))),
            li(p(raw("aliquando id"))))))

    assert(outputHTML.mkString === compileMarkdown(input).mkString)
  }

}

class HTMLSuite extends FunSuite {
  test("A single paragraph should be rendered as HTML.") {
    val input = "aaa"
    assert("<p>aaa</p>" === CommonMark(input))
  }
}
