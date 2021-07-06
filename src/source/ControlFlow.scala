
package com.mercerenies.befreak
package source

object ControlFlow:

  def repeat(body: Grid[Instruction], times: Int): Grid[Instruction] =
    (0 until times map { _ => body }).foldLeft(Command.empty) { _ ++ _ }

  // Assumes the loop counter is on top of the stack. We enter the
  // loop body in the upper-left going east. At this point, the loop
  // counter is on top of the main stack, its starting value is just
  // beneath it, and the control stack is left alone. We exit to the
  // upper-right, also going east.
  def countedLoop(body: Grid[Instruction]): Grid[Instruction] =
    sys.error("Not yet implemented")

end ControlFlow
