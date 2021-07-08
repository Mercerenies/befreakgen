
package com.mercerenies.befreak

import source._

import scala.collection.immutable.HashMap
import scala.language.implicitConversions

object Main:
  import source.ControlFlow.repeat

  def main(args: Array[String]) =

    val code = Command.pushNumber(2) ++ Command.pushNumber(3) ++ Algorithm.power
    val withPrint = Algorithm.program(code ++ Command.thenPrint)
    println(withPrint)
    println(Runner.runCode(withPrint.toString))
