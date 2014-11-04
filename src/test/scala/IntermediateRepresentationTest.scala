package com.acrussell.commonmark

import org.scalatest._, Matchers._
import scalaz._, Scalaz._

import com.acrussell.commonmark.ir._
import com.acrussell.commonmark.ir.documenttree._
import com.acrussell.commonmark.syntax._

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

    Parser(input) should equalDocument (expectedStructure)
  }

  test("The final document representation should be parsed from the block structure.") {
    val documentTree: Tree[Block] =
      Tree.node(Document(true), Stream(
        Tree.node(BlockQuote(true), Stream(
          Tree.leaf(Paragraph(false, Some("Lorem ipsum dolor\nsit amet."))),
          Tree.node(BulletList(true, true, '-'), Stream(
            Tree.node(ListItem(false), Stream(
              Tree.leaf(Paragraph(false, Some("Qui *quodsi iracundia*"))))),
            Tree.node(ListItem(true), Stream(
              Tree.leaf(Paragraph(true, Some("aliquando id")))))))))))

    val finalRepresentation: Tree[Block] =
      Tree.node(Document(false), Stream(
        Tree.node(BlockQuote(false), Stream(
          Tree.node(Paragraph(false, Some("Lorem ipsum dolor\nsit amet.")), Stream(
            Tree.leaf(Str(false, "Lorem ipsum dolor")),
            Tree.leaf(SoftBreak(false)),
            Tree.leaf(Str(false, "sit amet.")))),
          Tree.node(BulletList(false, true, '-'), Stream(
            Tree.node(ListItem(false), Stream(
              Tree.node(Paragraph(false, Some("Qui *quodsi iracundia*")), Stream(
                Tree.leaf(Str(false, "Qui ")),
                Tree.node(Emph(false), Stream(
                  Tree.leaf(Str(false, "quodsi iracundia")))))))),
            Tree.node(ListItem(false), Stream(
              Tree.node(Paragraph(false, Some("aliquando id")), Stream(
                Tree.leaf(Str(false, "aliquando id"))))))))))))

    Parser(documentTree) should equalDocument (finalRepresentation)
  }
}
