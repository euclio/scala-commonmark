package com.acrussell.commonmark

import com.acrussell.commonmark.ir._

object Parser {

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

  def apply(input: String) = parse(input)

  def parse(input: String) = {
    val document = new DocumentTree
    for (line <- input.split("\n")) {
      parseLine(document, line)
    }
    document.documentTree
  }

  def parseLine(document: DocumentTree, line: String): Unit = line match {
    case BlockQuotePattern(restOfLine) => {
      val childContainer = document.getChildContainer[BlockQuote](document.documentTree.loc)
      childContainer match {
        case Some(blockQuote) => ()
        case None => document.addChild(BlockQuote(true))
      }
      parseLine(document, restOfLine)
    }
    case ListItemPattern(delimiter, restOfLine) => {
      delimiter match {
        case "-" | "+" | "*" => document.addBulletListItem(delimiter.head)
      }
      parseLine(document, restOfLine)
    }
    case text => document.getLastOpenBlock.getLabel match {
        case p: Paragraph => document.addText(text)
        case _ => document.addChild(Paragraph(true, Some(text)))
    }
  }
}
