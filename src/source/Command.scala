
package com.mercerenies.befreak
package source

import scala.annotation.tailrec

object Command:

  def digits(n: Int): List[Int] =
    @tailrec
    def _rec(m: Int, acc: List[Int]): List[Int] =
      if m == 0 then
        acc
      else
        _rec(m / 10, (m % 10) :: acc)
    _rec(n, Nil)

  def xorNumber(n: Int): Grid[Instruction] =
    if n < 0 then
      throw Exception("Cannot currently handle negative numbers")
    else
      Grid.hstrip(Instruction.Space, digits(n) map { (x) => Instruction.Number(x) })

  def pushNumber(n: Int): Grid[Instruction] =
    Grid.singleton(Instruction.Space, Instruction.PushZero) hcat xorNumber(n)

end Command
