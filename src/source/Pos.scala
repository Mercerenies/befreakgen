
package com.mercerenies.befreak
package source

import scalaz._

case class Pos(x: Int, y: Int):

  def +(that: Pos) = Pos(this.x + that.x, this.y + that.y)

  def unary_- = Pos(- this.x, - this.y)

  def -(that: Pos) = this + -that

  def in(ul: Pos, lr: Pos) =
    this.x >= ul.x && this.y >= ul.y &&
    this.x < lr.x && this.y < lr.y

end Pos

object Pos:

  val Zero = Pos(0, 0)

  def min(a: Pos, b: Pos) =
    Pos(math.min(a.x, b.x), math.min(a.y, b.y))

  def max(a: Pos, b: Pos) =
    Pos(math.max(a.x, b.x), math.max(a.y, b.y))

end Pos
