
package com.mercerenies.befreak
package source

import ControlFlow.repeat

import scala.language.implicitConversions

object Algorithm:

  // Takes two values on top of the stack. At the end of the
  // computation, the gcd of the two values will be on top of the
  // stack, with the original two values beneath it unmodified.
  def gcd: Grid[Instruction] =
    ControlFlow.withReversed(
      Instruction.Dup ++
        ControlFlow.generalLoop(
          Instruction.Equal,
          Command.equalToZero,
          Instruction.Swapd ++ Instruction.Div ++ Instruction.Dig ++ Instruction.CtrlToMain ++ Instruction.Swap ++ Instruction.MainToCtrl.repeat(2) ++ Instruction.Bury,
        ),
      Instruction.MainToCtrl.repeat(2) ++ Instruction.Dup ++ Instruction.CtrlToMain.repeat(2)
    ) ++ Stack.moveToTop(2)

  // Takes two values on top of the stack. Puts lcm on top, leaves the
  // existing two values alone.
  def lcm: Grid[Instruction] =
    ControlFlow.withReversed(
      gcd ++ Instruction.MainToCtrl ++ Instruction.PushZero ++ Instruction.Swap ++ Instruction.Mul ++ Instruction.Swap ++ Instruction.CtrlToMain ++ Instruction.Div,
      Stack.copyToTop(2) ++ Stack.buryNDown(4),
    ) ++ Stack.moveToTop(2)

  // The whole program. Starts with @, runs once, and then terminates safely
  def program(body: Grid[Instruction]): Grid[Instruction] =
    val fullBody = Command.padding vcat body
    val lhs = Instruction.FMirror vcat Instruction.Halt
    val rhs = Instruction.BMirror vcat Instruction.FMirror
    lhs hcat fullBody hcat rhs

end Algorithm
