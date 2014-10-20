package com.acrussell.commonmark.ir

import scala.collection.mutable.Map

object State {
  val linkMap:Map[String,String] = Map()
}

sealed abstract class Block {
  def open: Boolean
}

sealed abstract class Container extends Block {
  def children: List[Block]
}

sealed abstract class Leaf extends Block

case class Document(
  override val open: Boolean,
  override val children: List[Block]) extends Container

case class BlockQuote(
  override val open: Boolean,
  override val children: List[Block]) extends Container

case class ListContainer(
  override val open: Boolean,
  override val children: List[Block]) extends Container

case class ListItem(
  override val open: Boolean,
  override val children: List[Block]) extends Container

case class HorizontalRule(override val open: Boolean) extends Leaf
case class AtxHeader(override val open: Boolean, val level: Integer) extends Leaf
case class SetextHeader(override val open: Boolean) extends Leaf
case class IndentedCode(override val open: Boolean) extends Leaf
case class FencedCode(override val open: Boolean) extends Leaf
case class Html(override val open: Boolean) extends Leaf
case class LinkReferenceDefinition(override val open: Boolean) extends Leaf
case class Paragraph(override val open: Boolean, val text: String) extends Leaf
case class BlankLine(override val open: Boolean) extends Leaf
