package com.acrussell.commonmark

import scala.io.Source

object CommonMark extends App {
  val fileName = args(0)
  val markdown = Source.fromFile(fileName).mkString

  val preprocessedMarkdown = Preprocessor(markdown)
  Console.println(preprocessedMarkdown)
}
