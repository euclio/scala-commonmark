package com.acrussell.commonmark.syntax

import scalaz._, Scalaz._

import com.acrussell.commonmark.ir._
import com.acrussell.commonmark.ir.documenttree._
import com.acrussell.commonmark.syntax.inlines._

package object blockstructure {
  /**
   * Finalizes the block structure of the document by closing all blocks,
   * resolving link references, and parsing paragraphs.
   *
   * @param input A tree of blocks that was parsed by the markdown parser.
   */
  def parseBlockStructure(input: Tree[Block]): Tree[Block] =
      transformParagraphs(input.map(closeBlock))

  /**
   * Parses all inlines inside the paragraph blocks of the input.
   */
  def transformParagraphs(input: Tree[Block]): Tree[Block] = input match {
    case Tree.Node(label, Stream.Empty) => label match {
      case p: Paragraph => parseInlines(input)
      case other => input
    }
    case Tree.Node(label, children) => {
      Tree.node(label, children.map(transformParagraphs))
    }
  }
}
