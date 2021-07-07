
package com.mercerenies.befreak

import source._

import scala.collection.immutable.HashMap
import scala.language.implicitConversions

object test:
  import source.ControlFlow.repeat

  def main(args: Array[String]) =

    val code = Command.pushNumber(4) ++ Command.pushNumber(6) ++ Algorithm.lcm
    val withPrint = Algorithm.program(code ++ Command.thenPrint)
    println(withPrint)
    println(Runner.runCode(withPrint.toString))
