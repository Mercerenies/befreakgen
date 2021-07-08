
package com.mercerenies.befreak
package source

case class Instruction(val representation: Char):

  override def toString: String = representation.toString

  def rotateLeft: Instruction =
    Instruction(representation match
      case '<' => 'v'
      case 'v' => '>'
      case '>' => '^'
      case '^' => '<'
      case '\\' => '/'
      case '/' => '\\'
      case ch => ch)

  def rotateRight: Instruction =
    Instruction(representation match
      case '<' => '^'
      case 'v' => '<'
      case '>' => 'v'
      case '^' => '>'
      case '\\' => '/'
      case '/' => '\\'
      case ch => ch)

  def mirror: Instruction =
    Instruction(representation match
      case '<' => '>'
      case 'v' => 'v'
      case '>' => '<'
      case '^' => '^'
      case '\\' => '/'
      case '/' => '\\'
      case ch => ch)

  def flip: Instruction =
    Instruction(representation match
      case '<' => '<'
      case 'v' => '^'
      case '>' => '>'
      case '^' => 'v'
      case '\\' => '/'
      case '/' => '\\'
      case ch => ch)

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

  extension (c: Char | Instruction) def toInstruction = c match
    case (c : Char) => Instruction(c)
    case (c: Instruction) => c

  given Conversion[Instruction, Grid[Instruction]] with
    def apply(i: Instruction) = Grid.singleton(Instruction.Space, i)

  extension (grid: Grid[Instruction])

    def rotateLeft: Grid[Instruction] = grid.rawRotateLeft.map(_.rotateLeft)

    def rotateRight: Grid[Instruction] = grid.rawRotateRight.map(_.rotateRight)

    // mirror and flip have been removed since they don't actually
    // maintain all of the invariants of the program (conditional
    // branches end up getting their semantics reversed, sadly)

    // def mirror: Grid[Instruction] = grid.rawMirror.map(_.mirror)

    // def flip: Grid[Instruction] = grid.rawFlip.map(_.flip)

end Instruction
