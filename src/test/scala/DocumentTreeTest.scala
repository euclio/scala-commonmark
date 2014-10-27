package com.acrussell.commonmark

import org.scalatest._

import scalaz._, Scalaz._

import com.acrussell.commonmark.ir._

class DocumentTreeSuite extends FunSuite {
  test("Text should be added to the last open block of a tree correctly.") {
    val documentTree = new DocumentTree(
      Tree.node(Document(true), Stream(
        Tree.leaf(Paragraph(true, Some("Test"))))))

    documentTree.addText("Test")

    val expectedTree = new DocumentTree(
      Tree.node(Document(true), Stream(
        Tree.leaf(Paragraph(true, Some("Test\nTest"))))))

    assert(documentTree == expectedTree)
  }

  test("The tree should specify whether the root is open or not.") {
    val documentTree = new DocumentTree
    assert(documentTree.getLastOpenBlock.getLabel == Document(true))
  }

  test("The tree should get the deepest open block.") {
    val documentTree = new DocumentTree(
      Tree.node(Document(true), Stream(
        Tree.leaf(Paragraph(true, Some("Test"))))))
    assert(documentTree.getLastOpenBlock.getLabel == Paragraph(true, Some("Test")))
  }

  test("The tree should get the deepest open block of a complicated structure.") {
    val documentTree = new DocumentTree(
      Tree.node(Document(true), Stream(
        Tree.node(BlockQuote(true), Stream(
          Tree.leaf(Paragraph(false, Some("Lorem ipsum dolor\nsit amet."))),
          Tree.node(BulletList(true, true, '-'), Stream(
            Tree.node(ListItem(false), Stream(
              Tree.leaf(Paragraph(false, Some("Qui *quodsi iracundia"))))),
            Tree.node(ListItem(false), Stream(
              Tree.leaf(Paragraph(true, Some("aliquando id"))))))))))))
    assert(documentTree.getLastOpenBlock.getLabel == Paragraph(true, Some("aliquando id")))
  }
}

class ParsingStrategySuite extends FunSuite {
  test("The document should start out as a empty, open document.") {
    val documentTree = new DocumentTree

    assert(documentTree == new DocumentTree(Tree.leaf(Document(true))))
  }

  test("Block quotes and paragraphs should be added correctly.") {
    val documentTree = new DocumentTree

    documentTree.addChild(BlockQuote(true))
    documentTree.addChild(Paragraph(true, None))
    documentTree.addText("Lorem ipsum dolor")

    val expectedTree = new DocumentTree(
      Tree.node(Document(true), Stream(
        Tree.node(BlockQuote(true), Stream(
          Tree.leaf(Paragraph(true, Some("Lorem ipsum dolor"))))))))
    assert(documentTree == expectedTree)
  }

  test("Lazy continuations should be added with newlines.") {
    val documentTree = new DocumentTree(
      Tree.node(Document(true), Stream(
        Tree.node(BlockQuote(true), Stream(
          Tree.leaf(Paragraph(true, Some("Lorem ipsum dolor"))))))))

    documentTree.addText("sit amet.")

    val expectedTree = new DocumentTree(
      Tree.node(Document(true), Stream(
        Tree.node(BlockQuote(true), Stream(
          Tree.leaf(Paragraph(true, Some("Lorem ipsum dolor\nsit amet."))))))))
    assert(documentTree == expectedTree)
  }

  test("A list should be created correctly.") {
    val documentTree = new DocumentTree(
      Tree.node(Document(true), Stream(
        Tree.node(BlockQuote(true), Stream(
          Tree.leaf(Paragraph(true, Some("Lorem ipsum dolor\nsit amet."))))))))

    documentTree.addBulletListItem('-')
    documentTree.addChild(Paragraph(true, None))
    documentTree.addText("Qui *quodsi iracundia*")

    val expectedTree = new DocumentTree(
      Tree.node(Document(true), Stream(
        Tree.node(BlockQuote(true), Stream(
          Tree.leaf(Paragraph(false, Some("Lorem ipsum dolor\nsit amet."))),
        Tree.node(BulletList(true, true, '-'), Stream(
          Tree.node(ListItem(true), Stream(
            Tree.leaf(Paragraph(true, Some("Qui *quodsi iracundia*"))))))))))))
    assert(documentTree == expectedTree)
  }

  test("A list should have items added to it correctly.") {
    val documentTree = new DocumentTree(
      Tree.node(Document(true), Stream(
        Tree.node(BlockQuote(true), Stream(
          Tree.leaf(Paragraph(false, Some("Lorem ipsum dolor\nsit amet."))),
        Tree.node(BulletList(true, true, '-'), Stream(
          Tree.node(ListItem(true), Stream(
            Tree.leaf(Paragraph(true, Some("Qui *quodsi iracundia*"))))))))))))

    documentTree.addBulletListItem('-')
    documentTree.addChild(Paragraph(true, None))
    documentTree.addText("aliquando id")

    val expectedTree = new DocumentTree(
      Tree.node(Document(true), Stream(
        Tree.node(BlockQuote(true), Stream(
          Tree.leaf(Paragraph(false, Some("Lorem ipsum dolor\nsit amet."))),
        Tree.node(BulletList(true, true, '-'), Stream(
          Tree.node(ListItem(false), Stream(
            Tree.leaf(Paragraph(false, Some("Qui *quodsi iracundia*"))))),
          Tree.node(ListItem(true), Stream(
            Tree.leaf(Paragraph(true, Some("aliquando id"))))))))))))

    assert(documentTree == expectedTree)
  }
}
