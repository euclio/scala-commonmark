package com.acrussell.commonmark

import org.scalatest._
import org.scalatest.Matchers._
import scalatags.Text.all._

import com.acrussell.commonmark.CommonMark._

class CommonMarkSuite extends FunSuite {
  test("A single paragraph should be rendered as HTML.") {
    val input = "aaa"
    assert("<p>aaa</p>" === CommonMark(input))
  }

}

class HTMLSuite extends FunSuite {
  test("A single paragraph should be compiled.") {
    val input = "aaa"
    assert(Stream(p("aaa")).mkString === compileMarkdown(input).mkString)
  }
}
