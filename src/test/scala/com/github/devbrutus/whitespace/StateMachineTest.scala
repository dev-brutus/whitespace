package com.github.devbrutus.whitespace

/**
 *
 */
object StateMachineTest extends App {
  private val stateMachine = new StateMachine(TranslatorTest.TRANSLATED_PROGRAM)

  while (stateMachine.isRun) {
    stateMachine.doStep()
  }
}
