package com.acrussell.commonmark


import com.acrussell.commonmark.ir._

import scalaz._

/**
 * Represents the Markdown document as a tree. The tree may be modified in
 * specific ways depending on the current line being parsed.
 */
class DocumentTree(baseTree: Tree[Block]) {
  import Scalaz._

  implicit val BlockEquals: Equal[Block] = Equal.equal(_ == _)
  def == (other: DocumentTree) = this.documentTree === other.documentTree

  var documentTree: Tree[Block] = baseTree

  def this() = this(Tree.leaf(Document(true)))

  /**
   * Adds text to the last open block of the tree.
   *
   * @param text The text to be added.
   */
  def addText(newText: String) = {
    val newLabel = getLastOpenBlock.getLabel match {
      case p: Paragraph => p.text match {
        case Some(text) => p.copy(text = Some(text + "\n" + newText))
        case None => p.copy(text = Some(newText))
      }
    }

    documentTree = getLastOpenBlock.setLabel(newLabel).toTree
  }

  def closeChildren(loc: TreeLoc[Block]) = {
    def closeBlock(block: Block): Block = {
      // Unfortunately, Scala isn't smart enough to realize that all subclasses
      // of loc have a copy method. There might be smarter way to do this,
      // though.
      block match {
        case d: Document => d.copy(open = false)
        case p: Paragraph => p.copy(open = false)
        case b: BlockQuote => b.copy(open = false)
        case l: ListItem => l.copy(open = false)
      }
    }

    val newTree = Tree.node(loc.getLabel, loc.tree.subForest.map(_.map(closeBlock)))
    loc.setTree(newTree)
  }


  /**
   * Attempts to add a child block to the current location. If it can't, it
   * attempts to add the child to the parent of the current location,
   * continuing up the tree until it reaches the document root.
   */
  private def addChildHelper(child: Block, location: TreeLoc[Block]) = {
    location.insertDownLast(Tree.leaf(child))
  }

  def addChild(child: Block) {
    documentTree = addChildHelper(child, getLastOpenBlock).toTree
  }

  def addBulletListItem(listMarker: Character) = {
    def getMatchingList(loc: TreeLoc[Block]): Option[TreeLoc[Block]] = {
      loc.getLabel match {
        case BulletList(true, _, matchListMarker) => {
          if (listMarker == matchListMarker) {
            Some(loc)
          } else {
            None
          }
        }
        case _ => loc.parent match {
          case Some(parent) => getMatchingList(parent)
          case None => None
        }
      }
    }

    documentTree = getMatchingList(getLastOpenBlock) match {
      case Some(listLoc) => {
        closeChildren(listLoc).insertDownLast(Tree.leaf(ListItem(true))).toTree
      }
      case None => {
        // TODO: Figure out how to determine loose vs. tight lists
        closeChildren(getLastOpenContainer).insertDownLast(Tree.node(
          BulletList(true, true, listMarker), Stream(
            Tree.leaf(ListItem(true))))).toTree
      }
    }
  }

  def getLastOpenContainer: TreeLoc[Block] = {
    def getLastOpenContainerHelper(loc: TreeLoc[Block]): TreeLoc[Block] = {
      loc.getLabel match {
        case c: Container => loc
        case l: Leaf => loc.left match {
          case Some(sibling) => getLastOpenContainerHelper(sibling)
          case None => loc.parent match {
            case Some(parent) => getLastOpenContainerHelper(parent)
            case None => throw new IllegalArgumentException("No open container in tree.")
          }
        }
      }
    }

    getLastOpenContainerHelper(getLastOpenBlock)
  }


  /**
   * Returns the rightmost, deepest open block in the tree. There is guaranteed
   * to be at least one open block in the tree.
   */
  def getLastOpenBlock: TreeLoc[Block] = {
    def getLastOpenBlockHelper(loc: TreeLoc[Block]): Option[TreeLoc[Block]] = {
      val childOpenBlock = loc.lastChild match {
        case Some(child) => getLastOpenBlockHelper(child)
        case None => None
      }

      childOpenBlock match {
        case Some(block) => Some(block)
        case None => if (loc.getLabel.open) {
          Some(loc)
        } else {
          None
        }
      }
    }

    getLastOpenBlockHelper(documentTree.loc).get
  }

  override def toString: String = {
    implicit val blockToString: Show[Block] = Show.showFromToString[Block]
    documentTree.drawTree
  }
}
