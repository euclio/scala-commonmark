package com.acrussell.commonmark

import org.scalatest._

import com.acrussell.commonmark.ir._

class IntermediateRepresentationSuite extends FunSuite {
  test("Block structure should be parsed correctly") {
    val input = """|> Lorem ipsum dolor
                   |sit amet.
                   |> - Qui *quodsi iracundia*
                   |> - aliquiando id""".stripMargin
    assert(Parser(input) ==
      Document(true, List(
        BlockQuote(false, List(
          Paragraph(false, "Lorem ipsum dolor\nsit amet."),
          ListContainer(true, List(
            ListItem(false, List(
              Paragraph(false, "Qui *quodsi iracundia*"))),
            ListItem(true, List(
              Paragraph(true, "aliquando id"))))))))))
  }
}
