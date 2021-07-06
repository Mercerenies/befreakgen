
package com.mercerenies.befreak

import source._

import scala.collection.immutable.HashMap
import scala.language.implicitConversions

object test:
  def main(args: Array[String]) =
    val code = Instruction.Halt ++ Command.pushNumber(93)
    println(code)
    println(Runner.runCode((code ++ Command.thenPrint).toString))
