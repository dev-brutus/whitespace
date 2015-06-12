package com.github.devbrutus.whitespace

import scala.collection.mutable

/**
 *
 */
class StateMachine(program: IndexedSeq[AnyRef]) {
  private val stack = mutable.Stack[Long]()
  private val heap = mutable.HashMap[Long, Long]()
  private val instructionStack = mutable.Stack[Int]()
  private var instruction: Int = -1
  private var isRun_ = true

  private val labels = program.zipWithIndex.map {
    case (('MRK, l: Long), index) => l -> index
    case _ => null
  } filter {
    _ != null
  } toMap

  def doStep() = {
    if (isRun_) {
      instruction = instruction + 1
      program(instruction) match {
        case s: Symbol =>
          s match {
            case 'DUP => duplicate
            case 'SWAP => swap
            case 'DISC => discard
            case 'ADD => add
            case 'SUB => sub
            case 'MUL => mul
            case 'IDIV => idiv
            case 'MOD => mod
            case 'RET => `return`()
            case 'END => end()

            case 'STOR => store()
            case 'RETR => retrieve()

            case 'WRCH => writeChar()
            case 'WRNM => writeNumber()
            case 'RDCH => readChar()
            case 'RDNM => readNumber()
          }
        case (s: Symbol, value: Long) =>
          s match {
            case 'PUSH => push(value)

            case 'COPY => copy(value.toInt)
            case 'SLID => slide(value.toInt)

            case 'MRK => null
            case 'CALL => call(value)
            case 'JMP => jump(value)
            case 'JZ => jumpIfZero(value)
            case 'JNEG => jumpIfNegative(value)
          }
      }
    }

    printState()
    isRun_
  }

  def printState(): Unit = {
    println(f"=== $instruction%05d ===")
    println(program(instruction))
    println("Stack:")
    stack.foreach(i => println(s"    $i"))
    println("Heap:")
    heap.foreach(i => println(s"    $i"))
    println(s"Run $isRun")
    println()
  }

  def isRun = isRun_

  def push(value: Long) = stack.push(value)

  def duplicate = stack.push(stack.head)

  def swap = {
    val v1 = stack.pop()
    val v2 = stack.pop()
    stack.push(v1, v2)
  }

  def discard = stack.pop()

  def copy(nth: Int) = stack.push(stack(nth.toInt))

  def slide(n: Int) = for (_ <- 0 to n) stack.pop()

  def math(f: (Long, Long) => Long) = stack.push(f(stack.pop(), stack.pop()))

  def add = math(_ + _)

  def sub = math(_ - _)

  def mul = math(_ * _)

  def idiv = math(_ / _)

  def mod = math(_ % _)

  def call(label: Long) = {
    instructionStack.push(instruction)
    jump(label)
  }

  def `return`() = {
    instruction = instructionStack.pop()
  }

  def jump(label: Long) = {
    instruction = labels(label) - 1
  }

  def jumpIfZero(label: Long) = {
    if (stack.head == 0) jump(label)
  }

  def jumpIfNegative(label: Long) = {
    if (stack.head < 0) jump(label)
  }

  def end() = isRun_ = false

  def store() = {
    val value = stack.head
    val address = stack(1)
    heap(address) = value
  }

  def retrieve() = {
    val address = stack.head
    stack.push(heap.getOrElse(address, 0))
  }

  def readNumber() = ???

  def readChar() = ???

  def writeNumber() = print(stack.head)

  def writeChar() = print(stack.head.asInstanceOf[Char])
}
