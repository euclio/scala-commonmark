package com.acrussell.commonmark.syntax

import scala.collection._
import scala.collection.mutable._

import scalaz._, Scalaz._

import com.acrussell.commonmark.ir._

package object inlines {
  def parseInlines(paragraph: Tree[Block]): Tree[Block] = {
    var newTree: TreeLoc[Block] = Tree.leaf[Block](Paragraph(false, None)).loc
    var currentStr: StringBuilder = new StringBuilder

    val innerText = paragraph.rootLabel.asInstanceOf[Paragraph].text.get
    val it = innerText.iterator.buffered
    while (it.hasNext) {
      val char = it.next
      char match {
        case '\n' => {
          val str = currentStr.mkString
          if (it.head == '\n') {
            // This is a hard break
            newTree = newTree
              .insertDownLast(Tree.leaf(Str(false, str))).parent.get
          } else {
            // This is a soft break
            newTree = newTree
              .insertDownLast(Tree.leaf(Str(false, str)))
              .insertRight(Tree.leaf(SoftBreak(false)))
          }
          currentStr.clear()
        }
        case '*' => {
          val str = currentStr.mkString

          // Are we in an emph?
          newTree = newTree.find(tree => tree.isLeaf && tree.isLast).get.getLabel match {
            case e: Emph => {
              newTree.insertDownLast(Tree.leaf(Str(false, str)))
            }
            case _ => {
              newTree
                .insertDownLast(Tree.leaf(Str(false, str)))
                .insertRight(Tree.leaf(Emph(false)))
            }
          }
          currentStr.clear()
        }
        case _ => {
          currentStr += char
        }
      }
    }

    if (currentStr.size > 0) {
      val currentInline = Str(false, currentStr.mkString)
      if (newTree.isRoot) {
        newTree = newTree.insertDownLast(Tree.leaf(currentInline))
      } else {
        newTree = newTree.insertRight(Tree.leaf(currentInline))
      }
    }

    newTree.toTree
  }
}

