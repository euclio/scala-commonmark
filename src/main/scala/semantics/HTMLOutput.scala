package com.acrussell.commonmark.semantics

import scalatags.Text.all._
import scalaz._, Scalaz._

import com.acrussell.commonmark.ir._

package object htmloutput {
  /**
   * Outputs HTML for an intermediate representation.
   *
   * @param ir The intermediate representation to parse.
   */
  def outputHTML(ir: Tree[Block]): Seq[Modifier] = {
    def transformToTags(ir: Tree[Block]): Modifier = {
      ir match {
        case Tree.Node(label, Stream.Empty) => createTag(label)
        case Tree.Node(label, children) => {
          getTag(label)(children.map(transformToTags))
        }
      }
    }

    ir.subForest.map(transformToTags)
  }

  def getTag(block: Block) = block match {
    case document: Document => html(_)
    case paragraph: Paragraph => p(_)
  }

  def createTag(block: Block) = block match {
    case str: Str => raw(str.text)
    case sb: SoftBreak => raw(" ")
  }
}
