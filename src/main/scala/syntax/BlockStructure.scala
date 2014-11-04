package com.acrussell.commonmark.syntax

import scalaz._, Scalaz._

import com.acrussell.commonmark.ir._

package object blockstructure {
  /**
   * Finalizes the block structure of the document by closing all blocks,
   * resolving link references, and parsing paragraphs.
   *
   * @param input A tree of blocks that was parsed by the markdown parser.
   */
  def parseBlockStructure(input: Tree[Block]): Tree[Block] = ???
}
