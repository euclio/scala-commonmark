package com.acrussell.commonmark

import scala.io.Source

import scalatags.Text.all._

import com.acrussell.commonmark.syntax.Preprocessor
import com.acrussell.commonmark.syntax.blockstructure._
import com.acrussell.commonmark.syntax.markdown._
import com.acrussell.commonmark.semantics.htmloutput._

object CommonMark extends App {
  def compileMarkdown(input: String): Seq[Modifier] = {
    val preprocessedMarkdown = Preprocessor(input)
    var ir = parseMarkdown(preprocessedMarkdown)
    var processedIR = parseBlockStructure(ir)
    outputHTML(processedIR)
  }

  def apply(input: String) = compileMarkdown(input).mkString("\n")

  val fileName = args(0)
  val markdown = Source.fromFile(fileName).mkString

  Console.println(apply(markdown))
}
