package com.acrussell.commonmark.syntax

import com.acrussell.commonmark.ir._
import com.acrussell.commonmark.ir.documenttree._
import com.acrussell.commonmark.syntax.blockstructure._
import com.acrussell.commonmark.syntax.markdown._

import scalaz._, Scalaz._

object Parser {
  /**
   * Parse the initial block structure from preprocessed markdown.
   */
  def apply(input: String): Tree[Block] = parseMarkdown(input)

  /**
   * Obtain the final block structure from the initial block structure.
   */
  def apply(input: Tree[Block]): Tree[Block] = parseBlockStructure(input)
}
