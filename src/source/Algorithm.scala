
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
          Instruction.Swapd ++ Instruction.Div ++ Instruction.Dig ++ Command.mainToCtrlDown ++ Instruction.Bury,
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

  // Takes two values on top of the stack: N and I (I on top). Puts
  // the number of times I divides into N on top.
  def countFactors: Grid[Instruction] =
    ControlFlow.withReversed(
      Instruction.Over ++ Instruction.PushZero ++
        ControlFlow.generalLoop(
          Stack.buryNDown(3) ++ Instruction.Swapd ++ Instruction.Equal ++ Instruction.Swapd ++ Stack.moveToTop(3),
          Stack.buryNDown(3) ++ Instruction.Swap ++ Instruction.Div ++ Instruction.Swap ++ Command.equalToZero ++ Instruction.ToggleCtrl ++ Instruction.Swap ++ Instruction.Mul ++ Instruction.Swap ++ Stack.moveToTop(3),
          Instruction.Increment ++ Stack.buryNDown(3) ++ Instruction.Swap ++ Instruction.Div ++ Instruction.Swap ++ Command.mainToCtrlDown ++ Instruction.Swap ++ Stack.moveToTop(3),
        ),
      Instruction.Dup ++ Stack.buryNDown(4),
    ) ++ Stack.moveToTop(2)

  // Given I on top of the stack, find the smallest positive X such
  // that 10^X mod I = 1. Pushes the result onto the stack
  def a006556: Grid[Instruction] =
    ControlFlow.withReversed(
      Command.pushNumber(10) ++ Instruction.Swap ++ Instruction.Div ++ Stack.moveToTop(2) ++ Command.mainToCtrlDown ++ Command.pushNumber(1) ++
        ControlFlow.generalLoop(
          Command.pushNumber(1) ++ Instruction.Equal ++ Command.popNumber(1),
          Instruction.Dig ++ Command.pushNumber(1) ++ Instruction.Equal ++ Command.popNumber(1) ++ Instruction.Bury,
          Instruction.Dig ++ Instruction.PushZero ++ Command.pushNumber(10) ++ Instruction.Mul ++ Command.popNumber(10) ++ Instruction.Dig ++ Instruction.Div ++ Instruction.Dig ++ Command.mainToCtrlDown ++ Instruction.Dig ++ Instruction.Increment,
        ),
      Instruction.Dup ++ Stack.buryNDown(3)
    ) ++ Instruction.Swap

  // Given I on top of the stack (assuming I is prime and not 2 or 5;
  // precondition not checked), produce A(I).
  def primeA: Grid[Instruction] =
    ControlFlow.withReversed(
      Instruction.PushZero ++ Instruction.MainToCtrl ++ Command.pushNumber(3) ++ Instruction.Equal ++ Command.popNumber(3) ++
        ControlFlow.ifStmt(
          Command.pushNumber(3),
          a006556,
        ) ++ Instruction.CtrlToMain.repeat(2) ++ Instruction.Swap ++ Instruction.MainToCtrl.repeat(2),
      Instruction.Dup ++ Instruction.Bury,
    ) ++ Instruction.Swap

  // Given A, B on the stack, raise A to the B power.
  def power: Grid[Instruction] =
    ControlFlow.withReversed(
      Command.pushNumber(1) ++ Instruction.Swap ++
        ControlFlow.countedLoop(
          Stack.moveToTop(2) ++ Stack.moveToTop(3) ++ Instruction.PushZero ++ Instruction.Swap ++ Instruction.Mul ++ Stack.buryNDown(3) ++ Stack.buryNDown(2)
        ),
      Instruction.Over ++ Stack.buryNDown(3)
    ) ++ Stack.moveToTop(2)

/*
  // Given N on top of the stack (gcd(10, N) = 1; precondition not
  // checked), produce A(N).
  def fullA: Grid[Instruction] =
    ControlFlow.withReversed(
      Command.pushNumber(1) ++ Command.pushNumber(2) ++
        ControlFlow.generalLoop(
          Command.pushNumber(2) ++ Instruction.Equal ++ Command.popNumber(2),
          Instruction.Dig ++ Command.pushNumber(1) ++ Instruction.Equal ++ Command.popNumber(1) ++ Instruction.Bury,

        ),
      _,
    )
*/

  // The whole program. Starts with @, runs once, and then terminates safely
  def program(body: Grid[Instruction]): Grid[Instruction] =
    val fullBody = Command.padding vcat body
    val lhs = Instruction.FMirror vcat Instruction.Halt
    val rhs = Instruction.BMirror vcat Instruction.FMirror
    lhs hcat fullBody hcat rhs

end Algorithm
