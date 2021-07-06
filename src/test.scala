
package com.mercerenies.befreak

import source._

import scala.collection.immutable.HashMap
import scala.language.implicitConversions

object test:
  def main(args: Array[String]) =
    val code = Instruction.Halt ++ Command.pushNumber(1) ++ Command.pushNumber(2) ++ Command.pushNumber(3) ++ Stack.copyToTop(2)
    val withPrint = code ++ Command.thenPrint
    println(code)
    println(Runner.runCode(withPrint.toString))
