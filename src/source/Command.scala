
package com.mercerenies.befreak
package source

import Instruction.toInstruction

import scala.language.implicitConversions
import scala.annotation.tailrec

object Command:

  import ControlFlow.repeat

  def digits(n: Int): List[Int] =
    @tailrec
    def _rec(m: Int, acc: List[Int]): List[Int] =
      if m == 0 then
        acc
      else
        _rec(m / 10, (m % 10) :: acc)
    _rec(n, Nil)

  def singleton(instr: Instruction | Char) = Grid.singleton(Instruction.Space, instr.toInstruction)

  def hstrip(seq: Seq[Instruction | Char]) = Grid.hstrip(Instruction.Space, seq map { _.toInstruction })

  def vstrip(seq: Seq[Instruction | Char]) = Grid.vstrip(Instruction.Space, seq map { _.toInstruction })

  def empty = Grid.empty(Instruction.Space)

  def padding = singleton(Instruction.Space)

  def xorNumber(n: Int): Grid[Instruction] =
    if n < 0 then
      throw Exception("Cannot currently handle negative numbers")
    else
      hstrip(digits(n) map { (x) => Instruction.Number(x) })

  def pushNumber(n: Int): Grid[Instruction] =
    singleton(Instruction.PushZero) hcat xorNumber(n)

  def popNumber(n: Int): Grid[Instruction] =
    xorNumber(n) hcat singleton(Instruction.PopZero)

  def equalToZero: Grid[Instruction] =
    Instruction.PushZero ++ Instruction.Equal ++ Instruction.PopZero

  def dip(inner: Grid[Instruction]): Grid[Instruction] =
    Instruction.MainToCtrl ++ inner ++ Instruction.CtrlToMain

  def mainToCtrlDown: Grid[Instruction] =
    Instruction.CtrlToMain ++ Instruction.Swap ++ Instruction.MainToCtrl.repeat(2)

  def thenPrint: Grid[Instruction] =
    Grid.fromRows(' ', List(raw"(ovs's[)84+84 01%01(v):v `w]v)", raw"  \c=c(=)           /  \=(=)/")).
      map { Instruction(_) }

end Command
