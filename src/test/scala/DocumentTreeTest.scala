package com.acrussell.commonmark

import org.scalatest._
import scalaz._
import Scalaz._

import com.acrussell.commonmark.ir._

class DocumentTreeSuite extends FunSuite {
  test("Text should be added to the last open block of a tree correctly.") {
    val documentTree = new DocumentTree(
      Tree.node(Document(true), Stream(
        Tree.leaf(Paragraph(true, "Test")))))

    documentTree.addText("Test")

    val expectedTree: Tree[Block] = Tree.node(Document(true), Stream(
      Tree.leaf(Paragraph(true, "Testtest"))))

    assert(documentTree.documentTree == expectedTree)
  }

  test("The tree should specify whether the root is open or not.") {
    val documentTree = new DocumentTree
    assert(documentTree.getLastOpenBlock.getLabel == Document(true))
  }

  test("The tree should get the deepest open block.") {
    val documentTree = new DocumentTree(
      Tree.node(Document(true), Stream(
        Tree.leaf(Paragraph(true, "Test")))))
    assert(documentTree.getLastOpenBlock.getLabel == Paragraph(true, "Test"))
  }

  test("The tree should get the deepest open block of a complicated structure.") {
    val documentTree = new DocumentTree(
      Tree.node(Document(true), Stream(
        Tree.node(BlockQuote(true), Stream(
          Tree.leaf(Paragraph(false, "Lorem ipsum dolor\nsit amet.")),
          Tree.node(ListContainer(true), Stream(
            Tree.node(ListItem(false), Stream(
              Tree.leaf(Paragraph(false, "Qui *quodsi iracundia")))),
            Tree.node(ListItem(false), Stream(
              Tree.leaf(Paragraph(true, "aliquando id")))))))))))
    assert(documentTree.getLastOpenBlock.getLabel == Paragraph(true, "aliquando id"))
  }
}
