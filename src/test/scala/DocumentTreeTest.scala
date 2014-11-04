package com.acrussell.commonmark.ir.documenttree

import com.acrussell.commonmark.ir._

import org.scalatest._, Matchers._
import scalaz._, Scalaz._

class DocumentTreeSuite extends FunSuite with DocumentMatchers {
  test("Text should be added to the last open block of a tree correctly.") {
    var documentTree: Tree[Block] =
      Tree.node(Document(true), Stream(
        Tree.leaf(Paragraph(true, Some("Test")))))

    documentTree = addText(documentTree.loc, "Test").toTree

    val expectedTree: Tree[Block] =
      Tree.node(Document(true), Stream(
        Tree.leaf(Paragraph(true, Some("Test\nTest")))))

    documentTree should equalDocument (expectedTree)
  }

  test("The tree should specify whether the root is open or not.") {
    assert(getLastOpenBlock(newDocument.loc).getLabel == Document(true))
  }

  test("The tree should get the deepest open block.") {
    val documentTree: Tree[Block] =
      Tree.node(Document(true), Stream(
        Tree.leaf(Paragraph(true, Some("Test")))))

    assert(getLastOpenBlock(documentTree.loc).getLabel == Paragraph(true, Some("Test")))
  }

  test("The tree should get the deepest open block of a complicated structure.") {
    val documentTree: Tree[Block] =
      Tree.node(Document(true), Stream(
        Tree.node(BlockQuote(true), Stream(
          Tree.leaf(Paragraph(false, Some("Lorem ipsum dolor\nsit amet."))),
          Tree.node(BulletList(true, true, '-'), Stream(
            Tree.node(ListItem(false), Stream(
              Tree.leaf(Paragraph(false, Some("Qui *quodsi iracundia"))))),
            Tree.node(ListItem(false), Stream(
              Tree.leaf(Paragraph(true, Some("aliquando id")))))))))))

    assert(getLastOpenBlock(documentTree.loc).getLabel == Paragraph(true, Some("aliquando id")))
  }
}

class ParsingStrategySuite extends FunSuite with DocumentMatchers {
  test("The document should start out as a empty, open document.") {
    newDocument should equalDocument (Tree.leaf(Document(true)))
  }

  test("Block quotes and paragraphs should be added correctly.") {
    var documentTree: TreeLoc[Block] = newDocument.loc

    documentTree = addChild(documentTree, BlockQuote(true))
    documentTree = addChild(documentTree, Paragraph(true, None))
    documentTree = addText(documentTree, "Lorem ipsum dolor")

    val expectedTree: Tree[Block] =
      Tree.node(Document(true), Stream(
        Tree.node(BlockQuote(true), Stream(
          Tree.leaf(Paragraph(true, Some("Lorem ipsum dolor")))))))

    documentTree.toTree should equalDocument (expectedTree)
  }

  test("Lazy continuations should be added with newlines.") {
    val documentTree: Tree[Block] =
      Tree.node(Document(true), Stream(
        Tree.node(BlockQuote(true), Stream(
          Tree.leaf(Paragraph(true, Some("Lorem ipsum dolor")))))))

    var modifiedTree = documentTree.loc

    modifiedTree = addText(modifiedTree, "sit amet.")

    val expectedTree: Tree[Block] =
      Tree.node(Document(true), Stream(
        Tree.node(BlockQuote(true), Stream(
          Tree.leaf(Paragraph(true, Some("Lorem ipsum dolor\nsit amet.")))))))

    modifiedTree.toTree should equalDocument (expectedTree)
  }

  test("A list should be created correctly.") {
    val documentTree: Tree[Block] =
      Tree.node(Document(true), Stream(
        Tree.node(BlockQuote(true), Stream(
          Tree.leaf(Paragraph(true, Some("Lorem ipsum dolor\nsit amet.")))))))

    var modifiedTree = documentTree.loc

    modifiedTree = addBulletListItem(modifiedTree, '-')
    modifiedTree = addChild(modifiedTree, Paragraph(true, None))
    modifiedTree = addText(modifiedTree, "Qui *quodsi iracundia*")

    val expectedTree: Tree[Block] =
      Tree.node(Document(true), Stream(
        Tree.node(BlockQuote(true), Stream(
          Tree.leaf(Paragraph(false, Some("Lorem ipsum dolor\nsit amet."))),
        Tree.node(BulletList(true, true, '-'), Stream(
          Tree.node(ListItem(true), Stream(
            Tree.leaf(Paragraph(true, Some("Qui *quodsi iracundia*")))))))))))

    modifiedTree.toTree should equalDocument (expectedTree)
  }

  test("A list should have items added to it correctly.") {
    val documentTree: Tree[Block] =
      Tree.node(Document(true), Stream(
        Tree.node(BlockQuote(true), Stream(
          Tree.leaf(Paragraph(false, Some("Lorem ipsum dolor\nsit amet."))),
        Tree.node(BulletList(true, true, '-'), Stream(
          Tree.node(ListItem(true), Stream(
            Tree.leaf(Paragraph(true, Some("Qui *quodsi iracundia*")))))))))))

    var modifiedTree = documentTree.loc

    modifiedTree = addBulletListItem(modifiedTree, '-')
    modifiedTree = addChild(modifiedTree, Paragraph(true, None))
    modifiedTree = addText(modifiedTree, "aliquando id")

    val expectedTree: Tree[Block] =
      Tree.node(Document(true), Stream(
        Tree.node(BlockQuote(true), Stream(
          Tree.leaf(Paragraph(false, Some("Lorem ipsum dolor\nsit amet."))),
        Tree.node(BulletList(true, true, '-'), Stream(
          Tree.node(ListItem(false), Stream(
            Tree.leaf(Paragraph(false, Some("Qui *quodsi iracundia*"))))),
          Tree.node(ListItem(true), Stream(
            Tree.leaf(Paragraph(true, Some("aliquando id")))))))))))

    modifiedTree.toTree should equalDocument (expectedTree)
  }
}
