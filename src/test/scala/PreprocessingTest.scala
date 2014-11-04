package com.acrussell.commonmark.syntax

import org.scalatest._

class PreprocessingSuite extends FunSuite {
  test("Tabs in lines are expanded to spaces, with a tab stop of 4 characters") {
    assert(Preprocessor("\tfoo\tbaz\t\tbim") == "    foo baz     bim")
    assert(Preprocessor("    a\ta\n    ὐ\ta") == "    a   a\n    ὐ   a")
  }

  test("All line terminators should be replaced with line feeds.") {
    assert(Preprocessor("a\rb\r\nc\nd") == "a\nb\nc\nd")
  }
}
