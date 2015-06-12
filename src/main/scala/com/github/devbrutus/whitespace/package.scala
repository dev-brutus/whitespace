package com.github.devbrutus

import java.io.InputStream

/**
 *
 */
package object whitespace {
  type TokensList = List[Char]

  val SPACE = ' '
  val TAB = '\t'
  val LF = '\n'

  val TOKENS = Set(SPACE, TAB, LF)

  implicit class Tokenizer(is: InputStream) {
    def toTokensList: TokensList = {
      Iterator.continually(is.read()).takeWhile(_ >= 0).map(_.asInstanceOf[Char]).filter(TOKENS).toList
    }
  }

}
