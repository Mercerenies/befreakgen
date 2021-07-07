
package com.mercerenies.befreak
package source

import scala.language.implicitConversions

// Stack manipulation primitives
object Stack:

  def moveToTop(n: Int): Grid[Instruction] =
    n match
      case 0 => Command.empty
      case 1 => Instruction.Swap
      case _ => Instruction.MainToCtrl ++ moveToTop(n - 1) ++ Instruction.CtrlToMain ++ Instruction.Swap

  // Inverse operation of moveToTop
  def buryNDown(n: Int): Grid[Instruction] =
    n match
      case 0 => Command.empty
      case 1 => Instruction.Swap
      case _ => Instruction.Swap ++ Instruction.MainToCtrl ++ buryNDown(n - 1) ++ Instruction.CtrlToMain

  def copyToTop(n: Int): Grid[Instruction] =
    n match
      case 0 => Instruction.Dup
      case _ => Instruction.MainToCtrl ++ copyToTop(n - 1) ++ Instruction.CtrlToMain ++ Instruction.Swap

end Stack
