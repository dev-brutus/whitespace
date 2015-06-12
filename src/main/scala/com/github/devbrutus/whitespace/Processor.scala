package com.github.devbrutus.whitespace

import java.io.InputStream

/**
 *
 */
class Processor(is: InputStream, val stateMachine: StateMachine) {
  def this(is: InputStream) = this(is, new StateMachine())

  private val tokenIterator: TokensList = is.toTokensList
}
