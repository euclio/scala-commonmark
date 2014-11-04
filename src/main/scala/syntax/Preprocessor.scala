package com.acrussell.commonmark.syntax

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.StringBuilder

/**
 * Prepares raw input for parsing.
 */
object Preprocessor {
  val TabSize = 4

  /**
   * Prepares raw Markdown for parsing by replacing tabs with spaces and
   * replacing all line terminators with the line feed character (LF, 0x0A).
   *
   * @param input The raw Markdown String
   * @return a String of standardized Markdown
   */
  def apply(input: String): String =
    (replaceLineTerminators _ andThen replaceTabsWithSpaces)(input)

  private def replaceLineTerminators(input: String) =
    """\r\n?|\n"""""".r.split(input).mkString("\n")

  private def replaceTabsWithSpaces(input: String) = {
    val outputBuffer = new ListBuffer[String]
    for (line <- input.split("\n")) {
      val lineBuilder = new StringBuilder
      for (char <- line) {
        char match {
          case '\t' => {
            val tabSpace = TabSize - lineBuilder.size % TabSize
            lineBuilder ++= " " * tabSpace
          }
          case _ => lineBuilder += char
        }
      }
      outputBuffer += lineBuilder.mkString
    }
    outputBuffer.mkString("\n")
  }
}
