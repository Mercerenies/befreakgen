
package com.mercerenies.befreak

import source._

import scala.collection.immutable.HashMap

object test:
  def main(args: Array[String]) =
    val code = Grid.singleton(Instruction.Space, Instruction.Halt) hcat Command.pushNumber(93) hcat Command.thenPrint
    println(code)
    println(Runner.runCode(code.toString))
