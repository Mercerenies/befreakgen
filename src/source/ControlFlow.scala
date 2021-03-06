
package com.mercerenies.befreak
package source

import scala.language.implicitConversions

object ControlFlow:

  extension (body: Grid[Instruction])
    def repeat(times: Int): Grid[Instruction] =
      (0 until times map { _ => body }).foldLeft(Command.empty) { _ ++ _ }

  def padToSameWidth(a: Grid[Instruction], b: Grid[Instruction]): (Grid[Instruction], Grid[Instruction]) =
    if b.width < a.width then
      val (b1, a1) = padToSameWidth(b, a)
      (a1, b1)
    else if b.width == a.width then
      (a, b)
    else
      val diff = b.width - a.width
      (a hcat Command.padding.repeat(diff), b)

  def padToWidth(a: Grid[Instruction], width: Int): Grid[Instruction] =
    val (a1, _) = padToSameWidth(a, Command.padding.repeat(width))
    a1

  // Creates a loop. First, we assume a control bit is on top of the
  // control stack. With that in mind, we enter the start condition,
  // then the end condition. If the control bit is 1, we exit the
  // loop. If not, we pop the control bit and enter the body, then
  // repeat.
  def generalLoop(
    startCondition: Grid[Instruction],
    endCondition: Grid[Instruction],
    body: Grid[Instruction],
  ): Grid[Instruction] =
    val topRow = (body hcat Instruction.Up).rotateLeft.rotateLeft
    val botRow = Instruction.BMirror hcat startCondition hcat endCondition
    val (pTopRow, pBotRow) = padToSameWidth(topRow, botRow)
    val base = (pTopRow hcat (Command.padding.repeat(topRow.height - 1).rotateRight vcat Instruction.Down)) vcat (pBotRow hcat Instruction.FMirror)
    if topRow.height == 1 then
      base
    else
      // Add a shim
      val lhs = Instruction.BMirror vcat Command.padding.repeat(topRow.height - 2).rotateRight vcat Instruction.BMirror
      val rhs = Instruction.FMirror vcat Command.padding.repeat(topRow.height - 2).rotateRight vcat Instruction.FMirror
      lhs hcat base hcat rhs

  // Assumes the loop counter is on top of the stack. We enter the
  // loop body in the upper-left going east. At this point, the loop
  // counter is on top of the main stack, its starting value is just
  // beneath it, and the control stack is left alone. We exit to the
  // upper-right, also going east. The loop will execute for N, N-1,
  // N-2, ..., 1. Note that, when the loop hits zero, the body will
  // not execute.
  def countedLoop(body: Grid[Instruction]): Grid[Instruction] =
    Instruction.Dup ++
      generalLoop(
        Instruction.Equal,
        Command.equalToZero,
        body ++ Instruction.Decrement,
      ) ++
      Instruction.PopZero

  // General-purpose if statement. If the value at the top of the
  // control stack is 1, runs the first block. If it's 0, runs the
  // second. The "control value" at the top of the control stack will
  // not be present during block execution, but it will be returned at
  // the end when the branch coalesces.
  def ifStmt(trueCase: Grid[Instruction], falseCase: Grid[Instruction]): Grid[Instruction] =
    val falseHeight = falseCase.height
    val center = falseCase vcat Command.padding vcat trueCase
    val lhs = Command.hstrip("\\/") vcat Command.padding.repeat(falseHeight - 1).rotateRight vcat Command.hstrip("\\<") vcat Command.hstrip(" \\")
    val rhs = Command.hstrip("\\/") vcat Command.padding.repeat(falseHeight - 1).rotateRight vcat Command.hstrip(">/") vcat Command.hstrip("/ ")
    lhs hcat center hcat rhs

  // Runs setup, then work, then setup in reverse. When setup runs,
  // there will be a single additional value on top of the control
  // stack, which should not be manipulated.
  def withReversed(setup: Grid[Instruction], work: Grid[Instruction]) =
    val topRow = Command.padding.repeat(setup.height - 1) ++ Instruction.Down ++ Instruction.ReverseMode
    val fullSetup = Command.padding hcat (topRow vcat setup.rotateRight)
    val paddedWork = padToWidth(work, 2)
    val controlRow = Command.padding.repeat(setup.height - 1) ++ Instruction.FMirror ++ Instruction.Up ++ Instruction.ReverseMode ++ Command.padding.repeat(paddedWork.width - 2) ++ Instruction.BMirror
    val workRow = Command.padding.repeat(setup.height - 1) ++ Instruction.BMirror ++ paddedWork ++ Instruction.FMirror
    fullSetup vcat controlRow vcat workRow

end ControlFlow
