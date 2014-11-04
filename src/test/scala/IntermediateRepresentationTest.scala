package com.acrussell.commonmark

import org.scalatest._, Matchers._
import scalaz._, Scalaz._

import com.acrussell.commonmark.ir._
import com.acrussell.commonmark.ir.documenttree._

class IntermediateRepresentationSuite extends FunSuite with DocumentMatchers {
  test("Block structure should be parsed correctly") {
    val input = """|> Lorem ipsum dolor
                   |sit amet.
                   |> - Qui *quodsi iracundia*
                   |> - aliquando id""".stripMargin

    val expectedStructure: Tree[Block] =
      Tree.node(Document(true), Stream(
        Tree.node(BlockQuote(true), Stream(
          Tree.leaf(Paragraph(false, Some("Lorem ipsum dolor\nsit amet."))),
          Tree.node(BulletList(true, true, '-'), Stream(
            Tree.node(ListItem(false), Stream(
              Tree.leaf(Paragraph(false, Some("Qui *quodsi iracundia*"))))),
            Tree.node(ListItem(true), Stream(
              Tree.leaf(Paragraph(true, Some("aliquando id")))))))))))

    val output = Parser(input)

    Parser(input) should equalDocument (expectedStructure)
  }
}
