package com.acrussell.commonmark.ir.documenttree

import org.scalatest._, matchers._
import scalaz._, Scalaz._

import com.acrussell.commonmark.ir._

trait DocumentMatchers {
  /**
   * This matcher compares document trees for equality. Particularly, this
   * matcher uses the Tree.drawTree instead of Tree.toString for more
   * informative error output.
   */
  class DocumentTreeShouldBeEqualMatcher(expectedTree: Tree[Block]) extends Matcher[Tree[Block]] {
    def apply(left: Tree[Block]) = {
      MatchResult(
        left === expectedTree,
        s"""\n${left.drawTree} did not equal\n${expectedTree.drawTree}""",
        s"""\n${left.drawTree} did equal \n${expectedTree.drawTree}"""
      )
    }
  }

  def equalDocument(expectedTree: Tree[Block]) =
    new DocumentTreeShouldBeEqualMatcher(expectedTree)
}

object DocumentMatchers extends DocumentMatchers
