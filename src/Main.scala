
package com.mercerenies.befreak

import source._

import scala.collection.immutable.HashMap
import scala.language.implicitConversions

object Main:
  import source.ControlFlow.repeat

  def main(args: Array[String]) =

    //val code = Command.pushNumber(41) ++ Algorithm.fullA
    //val withPrint = Algorithm.program(code ++ Command.thenPrint)
    println(Algorithm.fullProgram)
    //println(Runner.runCode(Algorithm.fullProgram.toString))
