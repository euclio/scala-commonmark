package com.acrussell.commonmark

import com.acrussell.commonmark.ir._

object Parser {

//  private object DocumentManager {
//    private var document = Document(true, List.empty)
//
//    def addChild(cls: Class) {
//
//    }
//  }

  /**
   * A block quote marker consists of 0-3 spaces of initial indent, plus
   *    (a) the character '>' together with a following space, or
   *    (b) a single character '>' not followed by a space
   */
  val BlockQuotePattern = """^ {0,3}> ?(.*)$""".r

  def apply(input: String) = parse(input)

  def parse(input: String) = {
    for (line <- input.split("\n")) {
//      parseLine(document, line)
    }
  }

  def parseLine(document: Document, line: String): Unit = line match {
    case BlockQuotePattern(text) => {
//      DocumentManager.addChild(BlockQuote())
//      parseLine(line)
    }
    case text => ()
  }

  def parseBlockQuote(text: String) = {
 //   BlockQuote(true, List(Paragraph(true, text)))
  }

  def addChild(document: Document) = {

  }

  private def appendToLastOpenBlock(block: Block, text: String) {
  }
}
