
package com.mercerenies.befreak
package source

case class Instruction(val representation: Char):
  override def toString: String = representation.toString
end Instruction

object Instruction:
  def Number(n: Int) = Instruction((n + '0').toChar)
  def PushZero = Instruction('(')
  def PopZero = Instruction(')')
  def MainToCtrl = Instruction('[')
  def CtrlToMain = Instruction(']')
  def SwapCtrl = Instruction('$')
  def Write = Instruction('w')
  def Read = Instruction('r')
  def Increment = Instruction('\'')
  def Decrement = Instruction('`')
  def Add = Instruction('+')
  def Sub = Instruction('-')
  def Div = Instruction('%')
  def Mul = Instruction('*')
  def Not = Instruction('~')
  def And = Instruction('&')
  def Or = Instruction('|')
  def Xor = Instruction('#')
  def LeftRot = Instruction('{')
  def RightRot = Instruction('}')
  def ToggleCtrl = Instruction('!')
  def Equal = Instruction('=')
  def LT = Instruction('l')
  def GT = Instruction('g')
  def Swap = Instruction('s')
  def Dig = Instruction('d')
  def Bury = Instruction('b')
  def Flip = Instruction('f')
  def Swapd = Instruction('c')
  def Over = Instruction('o')
  def Under = Instruction('u')
  def Dup = Instruction(':')
  def Dedup = Instruction(';')
  def StringMode = Instruction('"')
  def ReverseMode = Instruction('?')
  def Halt = Instruction('@')
  def BMirror = Instruction('\\')
  def FMirror = Instruction('/')
  def Left = Instruction('>')
  def Right = Instruction('<')
  def Down = Instruction('v')
  def Up = Instruction('^')
  def Space = Instruction(' ')
end Instruction
