
package com.mercerenies.befreak

import source.{Instruction, Grid, Pos}

import scala.collection.immutable.HashMap

def test1(a: Object) = a

object test:
  def main(args: Array[String]) =
    val grid = Grid.hstrip(' ', "ABCDE")
    println(grid)
    println("---")
    println(grid.insert(grid.mirror shift Pos(4, 0), Pos(0, 2)))
    println("---")
    println(grid vcat grid.mirror)
