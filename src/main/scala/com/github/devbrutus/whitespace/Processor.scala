package com.github.devbrutus.whitespace

import java.io.InputStream

/**
 *
 */
class Processor(is: InputStream) {

  private val stateMachine = new StateMachine(Translator(is.toTokensList).toIndexedSeq)

  def apply(): Unit = {
    while (stateMachine.isRun) {
      stateMachine.doStep()
    }
  }
}
