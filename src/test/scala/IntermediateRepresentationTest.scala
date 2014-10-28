package com.acrussell.commonmark

import org.scalatest._
import scalaz._, Scalaz._

import com.acrussell.commonmark.ir._

class IntermediateRepresentationSuite extends FunSuite {
  test("Block structure should be parsed correctly") {
    val input = """|> Lorem ipsum dolor
                   |sit amet.
                   |> - Qui *quodsi iracundia*
                   |> - aliquiando id""".stripMargin

    val expectedStructure: Tree[Block] =
      Tree.node(Document(true), Stream(
        Tree.node(BlockQuote(true), Stream(
          Tree.leaf(Paragraph(false, "Lorem ipsum dolor\nsit amet.")),
          Tree.node(ListContainer(true), Stream(
            Tree.node(ListItem(false), Stream(
              Tree.leaf(Paragraph(false, "Qui *quodsi iracundia")))),
            Tree.node(ListItem(false), Stream(
              Tree.leaf(Paragraph(true, "aliquando id"))))))))))

    assert(Parser(input) == expectedStructure)
  }
}
