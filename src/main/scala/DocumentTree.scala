package com.acrussell.commonmark


import com.acrussell.commonmark.ir._

import scalaz._

/**
 * Represents the Markdown document as a tree. The tree may be modified in
 * specific ways depending on the current line being parsed.
 */
class DocumentTree {
  import Scalaz._

  var documentTree: Tree[Block] = Tree.leaf(Paragraph(true, "HI"))
  var lastOpenBlock: TreeLoc[Block] = documentTree.loc

  /**
   * Adds text to the last open block of the tree.
   *
   * @param text The text to be added.
   */
  def addText(newText: String) = {
    val openBlockLabel = lastOpenBlock.getLabel
    val newOpenBlock = openBlockLabel match {
      case p:Paragraph => p.copy(text = p.text + newText)
    }
    implicit def blockShow = Show.showFromToString[Block]

    lastOpenBlock.setLabel(newOpenBlock)
    println("GETLABEL")
    println(lastOpenBlock.toTree.draw)
    println("GETLABEL")
    documentTree = lastOpenBlock.root.toTree
  }
}
