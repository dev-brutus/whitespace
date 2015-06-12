package com.github.devbrutus.whitespace

/**
 *
 */
object TranslatorTest extends App {
  val PROGRAM = List(
    SPACE, SPACE, SPACE, TAB, LF, // 	Put a 1 on the stack
    LF, SPACE, SPACE, SPACE, TAB, SPACE, SPACE, SPACE, SPACE, TAB, TAB, LF, // 	Set a Label at this point
    SPACE, LF, SPACE, // 	Duplicate the top stack item
    TAB, LF, SPACE, TAB, // 	Output the current value
    SPACE, SPACE, SPACE, TAB, SPACE, TAB, SPACE, LF, // 	Put 10 (newline) on the stack...
    TAB, LF, SPACE, SPACE, // 	...and output the newline
    SPACE, SPACE, SPACE, TAB, LF, // 	Put a 1 on the stack
    TAB, SPACE, SPACE, SPACE, // 	Addition. This increments our current value.
    SPACE, LF, SPACE, // 	Duplicate that value so we can test it
    SPACE, SPACE, SPACE, TAB, SPACE, TAB, TAB, LF, // 	Push 11 onto the stack
    TAB, SPACE, SPACE, TAB, // 	Subtraction. So if we've reached the end, we have a zero on the stack.
    LF, TAB, SPACE, SPACE, TAB, SPACE, SPACE, SPACE, TAB, SPACE, TAB, LF, // 	If we have a zero, jump to the end
    LF, SPACE, LF, SPACE, TAB, SPACE, SPACE, SPACE, SPACE, TAB, TAB, LF, // 	Jump to the start
    LF, SPACE, SPACE, SPACE, TAB, SPACE, SPACE, SPACE, TAB, SPACE, TAB, LF, // 	Set the end label
    SPACE, LF, LF, // 	Discard our accumulator, to be tidy
    LF, LF, LF // 	Finish
  )

  val listing = Translator(PROGRAM).map {
    case s: Symbol => s"$s;"
    case (s: Symbol, n: Long) => s"${s.toString().padTo(4, " ")} $n;"
  }

  println(listing.mkString("\n"))

}
