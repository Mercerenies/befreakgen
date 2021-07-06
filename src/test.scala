
package com.mercerenies.befreak

import source._

import scala.collection.immutable.HashMap
import scala.language.implicitConversions

object test:
  def main(args: Array[String]) =
    val code = Instruction.Halt ++ ControlFlow.repeat(Command.pushNumber(1), 2)
    val withPrint = code ++ Command.thenPrint
    println(code)
    println(Runner.runCode(withPrint.toString))
