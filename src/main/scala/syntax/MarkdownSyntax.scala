package com.acrussell.commonmark.syntax

import scalaz._, Scalaz._

import com.acrussell.commonmark.ir._
import com.acrussell.commonmark.ir.documenttree._

/**
 * Provides functions for parsing markdown syntax.
 */
package object markdown {
  /**
   * A block quote marker consists of 0-3 spaces of initial indent, plus
   *    (a) the character '>' together with a following space, or
   *    (b) a single character '>' not followed by a space
   */
  val BlockQuotePattern = """^ {0,3}> ?(.*)$""".r

  /**
   * A list item is determined by a marker that is either
   *    (a) '-', '+', or '*'
   *    (b) One or more digits followed by either a '.' or a ')'
   * The marker is then followed by zero to five spaces.
   */
  val ListItemPattern = """^(-|\+|\*|\d+(?:\.|\))) {0,5}(.*)$""".r

  def parseMarkdown(input: String): Tree[Block] =
    input.split("\n")
      .foldLeft(newDocument.loc)((document, line) => parseLine(document, line))
      .toTree

  def parseLine(document: TreeLoc[Block], line: String): TreeLoc[Block] = line match {
    case BlockQuotePattern(restOfLine) => {
      val childContainer = getChildContainer[BlockQuote](document)
      childContainer match {
        case Some(blockQuote) => parseLine(blockQuote, restOfLine)
        case None => parseLine(addChild(document, BlockQuote(true)), restOfLine)
      }
    }
    case ListItemPattern(delimiter, restOfLine) => {
      delimiter match {
        case "-" | "+" | "*" => parseLine(addBulletListItem(document, delimiter.head), restOfLine)
      }
    }
    case text if !text.isEmpty => getLastOpenBlock(document).getLabel match {
        case p: Paragraph => addText(document, text).root
        case _ => addChild(document, Paragraph(true, Some(text))).root
    }
    case _ => document.root     // Nothing left to parse
  }
}
