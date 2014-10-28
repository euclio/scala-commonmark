package com.acrussell.commonmark


import com.acrussell.commonmark.ir._

import scalaz._

/**
 * Represents the Markdown document as a tree. The tree may be modified in
 * specific ways depending on the current line being parsed.
 */
class DocumentTree(baseTree: Tree[Block]) {
  import Scalaz._

  var documentTree: Tree[Block] = baseTree

  def this() = this(Tree.leaf(Document(true)))

  /**
   * Adds text to the last open block of the tree.
   *
   * @param text The text to be added.
   */
  def addText(newText: String) = {
    val newLabel = getLastOpenBlock.getLabel match {
      case p:Paragraph => p.copy(text = p.text + newText)
    }

    documentTree = getLastOpenBlock.setLabel(newLabel).toTree
  }

  private def getLastOpenBlockHelper(loc: TreeLoc[Block]): Option[TreeLoc[Block]] = {
    loc.lastChild match {
      case Some(child) => Some(child)
      case None => {
        if (loc.getLabel.open){
          Some(loc)
        } else {
          loc.left match {
            case Some(sibling) => getLastOpenBlockHelper(sibling)
            case None => None
          }
        }
      }
    }
  }

  /**
   * Returns the rightmost, deepest open block in the tree. There is guaranteed
   * to be at least one open block in the tree.
   */
  def getLastOpenBlock: TreeLoc[Block] = getLastOpenBlockHelper(documentTree.loc).get
}
