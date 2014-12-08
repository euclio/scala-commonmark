package com.acrussell.commonmark.ir

import scala.reflect.ClassTag

import scalaz._, Scalaz._

/**
 * Provides functions for manipulating the representation of the Markdown
 * document as a tree.
 */
package object documenttree {
  /**
   * Creates a new document tree. An empty document tree contains a single
   * Document block that is open.
   */
  def newDocument: Tree[Block] = Tree.leaf(Document(true))

  /**
   * Adds text to the last open block of the tree.
   *
   * @param text The text to be added.
   */
  def addText(loc: TreeLoc[Block], newText: String): TreeLoc[Block] =
    getLastOpenBlock(loc).modifyLabel(_ match {
      case p: Paragraph => p.text match {
        case Some(text) => p.copy(text = Some(text + "\n" + newText))
        case None => p.copy(text = Some(newText))
      }
    })

  /**
   * Marks a block as closed.
   *
   * @param block The block to close.
   */
  def closeBlock(block: Block): Block = {
    // Unfortunately, Scala isn't smart enough to realize that all subclasses
    // of Block have a copy method, so we have to match all cases
    // individually.
    //
    // However, there might be smarter way to do this.
    block match {
      case d: Document => d.copy(open = false)
      case p: Paragraph => p.copy(open = false)
      case b: BlockQuote => b.copy(open = false)
      case l: ListItem => l.copy(open = false)
      case l: BulletList => l.copy(open = false)
    }
  }

  /**
   * Closes all children of a given tree location.
   *
   * @param loc The node to close the children of.
   */
  def closeChildren(loc: TreeLoc[Block]): TreeLoc[Block] = {
    val newTree = Tree.node(loc.getLabel, loc.tree.subForest.map(_.map(closeBlock)))
    loc.setTree(newTree)
  }

  /**
   * Attempts to add a child block to the last open block. If the current block
   * is closed, the function will attempt to add the child to the current
   * location's parent instead.
   *
   * All blocks can be added to the document root, so the block is guaranteed
   * to be added.
   *
   * @param child The Block to add to the document.
   */
  def addChild(document: TreeLoc[Block], child: Block): TreeLoc[Block] = {
    def addChildHelper(loc: TreeLoc[Block], child: Block) =
      loc.insertDownLast(Tree.leaf(child))

    addChildHelper(getLastOpenBlock(document), child)
  }

  /**
   * Adds a bullet list item to the current location. If there is an open list
   * container with a matching marker, the list item is added to that
   * container. Otherwise, a new list is created.
   *
   * @param listMarker The character that the current list uses as a list
   * marker.
   */
  def addBulletListItem(loc: TreeLoc[Block], listMarker: Character): TreeLoc[Block] = {
    /**
     * Recursively finds the deepest list container in the tree that matches
     * the current list item.
     */
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

    getMatchingList(getLastOpenBlock(loc)) match {
      case Some(listLoc) => {
        closeChildren(listLoc).insertDownLast(Tree.leaf(ListItem(true)))
      }
      case None => {
        // TODO: Figure out how to determine loose vs. tight lists
        val newList: Tree[Block] = Tree.node(
          BulletList(true, true, listMarker), Stream(
            Tree.leaf(ListItem(true))))

        closeChildren(getLastOpenContainer(loc)).insertDownLast(newList)
      }
    }
  }

  /**
   * Optionally returns any children of the current node that are both open and
   * an instance of Block type T.
   *
   * @param loc The location to look for children of.
   * @param T The block type to find
   */
  def getChildContainer[T <: Block: ClassTag](loc: TreeLoc[Block]): Option[TreeLoc[Block]] = {
    // If we don't keep track of the class tag, then the type information is
    // erased at runtime.
    val clazz = implicitly[ClassTag[T]].runtimeClass
    loc.findChild(tree => tree.rootLabel.open && clazz.isInstance(tree.rootLabel))
  }

  /**
   * Returns the rightmost, deepest open block in the tree. There is guaranteed
   * to be at least one open block in the tree.
   */
  def getLastOpenBlock(loc: TreeLoc[Block]): TreeLoc[Block] = {
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

    getLastOpenBlockHelper(loc).get
  }

  def getLastOpenContainer(loc: TreeLoc[Block]): TreeLoc[Block] = {
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

    getLastOpenContainerHelper(getLastOpenBlock(loc))
  }
}
