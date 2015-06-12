package com.github.devbrutus.whitespace

/**
 *
 */
object Translator {

  type ProcessedPart = (AnyRef, TokensList)

  private def stackManipulation(tokens: TokensList): ProcessedPart = {
    val tokensTail = tokens.tail
    tokens.headOption match {
      case None => null
      case Some(SPACE) => readNumber('PUSH, tokensTail) // Push the number onto the stack
      case Some(LF) => tokensTail.headOption match {
        case None => null
        case Some(SPACE) => 'DUP -> tokensTail.tail // Duplicate the top item on the stack
        case Some(TAB) => 'SWAP -> tokensTail.tail // Swap the top two items on the stack
        case Some(LF) => 'DISC -> tokensTail.tail // Discard the top item on the stack
      }
      case Some(TAB) => tokensTail.headOption match {
        case Some(SPACE) => readNumber('COPY, tokensTail.tail) // Copy the nth item on the stack (given by the argument) onto the top of the stack
        case Some(LF) => readNumber('SLID, tokensTail.tail) // Slide n items off the stack, keeping the top item
        case _ => null
      }
    }
  }

  private def arithmetic(tokens: TokensList): ProcessedPart = {
    val tokensTail = tokens.tail
    tokens.headOption match {
      case Some(SPACE) => tokensTail.headOption match {
        case None => null
        case Some(SPACE) => 'ADD -> tokensTail.tail // Addition
        case Some(TAB) => 'SUB -> tokensTail.tail // Subtraction
        case Some(LF) => 'MUL -> tokensTail.tail // Multiplication
      }
      case Some(TAB) => tokensTail.headOption match {
        case Some(SPACE) => 'IDIV -> tokensTail.tail // Integer Division
        case Some(TAB) => 'MOD -> tokensTail.tail // Modulo
        case _ => null
      }
      case _ => null
    }
  }

  private def flowControl(tokens: TokensList): ProcessedPart = {
    val tokensTail = tokens.tail
    tokens.headOption match {
      case None => null
      case Some(SPACE) => tokensTail.headOption match {
        case None => null
        case Some(SPACE) => readNumber('MRK, tokensTail.tail) // Mark a location in the program
        case Some(TAB) => readNumber('CALL, tokensTail.tail) // Call a subroutine
        case Some(LF) => readNumber('JMP, tokensTail.tail) // Jump unconditionally to a label
      }
      case Some(TAB) => tokensTail.headOption match {
        case Some(SPACE) => readNumber('JZ, tokensTail.tail) // Jump to a label if the top of the stack is zero
        case Some(TAB) => readNumber('JNEG, tokensTail.tail) // Jump to a label if the top of the stack is negative
        case Some(LF) => 'RET -> tokensTail.tail // End a subroutine and transfer control back to the caller
      }
      case Some(LF) => 'END -> tokensTail.tail // End the program
    }
  }


  private def heapAccess(tokens: TokensList): ProcessedPart = {
    val tokensTail = tokens.tail
    tokens.headOption match {
      case SPACE => 'STOR -> tokensTail // Store
      case TAB => 'RETR -> tokensTail // Retrieve
    }
  }

  private def io(tokens: TokensList): ProcessedPart = {
    val tokensTail = tokens.tail
    tokens.headOption match {
      case Some(SPACE) => tokensTail.headOption match {
        case Some(SPACE) => 'WRCH -> tokensTail.tail // Output the character at the top of the stack
        case Some(TAB) => 'WRNM -> tokensTail.tail // Output the number at the top of the stack
        case _ => null
      }
      case Some(TAB) => tokensTail.headOption match {
        case Some(SPACE) => 'RDCH -> tokensTail.tail // Read a character and place it in the location given by the top of the stack
        case Some(TAB) => 'RDNM -> tokensTail.tail // Read a number and place it in the location given by the top of the stack
        case _ => null
      }
      case _ => null
    }
  }

  def apply(tokens: TokensList): Stream[AnyRef] = {
    val tokensTail = tokens.tail
    val processedPart = tokens.headOption match {
      case None => null
      case Some(SPACE) => stackManipulation(tokensTail)
      case Some(LF) => flowControl(tokensTail)
      case Some(TAB) => tokensTail.headOption match {
        case None => null
        case Some(SPACE) => arithmetic(tokensTail.tail)
        case Some(TAB) => heapAccess(tokensTail.tail)
        case Some(LF) => io(tokensTail.tail)
      }
    }

    if (processedPart == null) Stream.empty
    else Stream.cons(processedPart._1, apply(processedPart._2))
  }

  private def readNumber(symbol: Symbol, tokens: TokensList): ((Symbol, Long), TokensList) = {
    val (numberData, tail) = tokens.span(_ == LF)
    val number = numberData.tail.foldLeft(0L)((number, ch) => {
      val result = number * 2
      if (ch == TAB) result + 1
      else result
    })

    val result = numberData.head match {
      case TAB => -number
      case _ => number
    }

    ((symbol, result), tail)
  }
}
