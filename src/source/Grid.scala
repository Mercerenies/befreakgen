
package com.mercerenies.befreak
package source

import scala.collection.immutable.HashMap
import scala.language.postfixOps

// Unbounded grid with an origin point. Y points down.
class Grid[+A](val default: A, val impl: HashMap[Pos, A] = HashMap()):

  def apply(pos: Pos): A =
    impl.applyOrElse(pos, (_) => default)

  def filter(pred: ((Pos, A)) => Boolean): Grid[A] =
    Grid(default, impl filter pred)

  def shift(pos: Pos): Grid[A] =
    Grid(default, impl map { (k, v) => (k + pos, v) })

  def slice(upperLeft: Pos, lowerRight: Pos): Grid[A] =
    this filter { (k, _) => k.in(upperLeft, lowerRight) } shift upperLeft

  def insert[B](grid: Grid[B], origin: Pos): Grid[A | B] =
    val other = grid.shift(origin).impl
    Grid(default, impl.merged(other) { (_, rhs) => rhs })

  def upperLeft: Pos =
    impl.keys.fold(Pos.Zero)(Pos.min)

  def lowerRight: Pos =
    impl.keys.fold(Pos.Zero)(Pos.max) + Pos(1, 1)

  def rotateRight: Grid[A] =
    Grid(default, impl map { (k, v) => (Pos(- k.y, k.x), v) })

  def rotateLeft: Grid[A] =
    Grid(default, impl map { (k, v) => (Pos(k.y, - k.x), v) })

  def mirror: Grid[A] =
    Grid(default, impl map { (k, v) => (Pos(- k.x, k.y), v) })

  def flip: Grid[A] =
    Grid(default, impl map { (k, v) => (Pos(k.x, - k.y), v) })

  def hcat[B](that: Grid[B]): Grid[A | B] =
    this.insert(that, Pos(this.lowerRight.x - that.upperLeft.x, this.upperLeft.y - that.upperLeft.y))

  def vcat[B](that: Grid[B]): Grid[A | B] =
    this.insert(that, Pos(this.upperLeft.x - that.upperLeft.x, this.lowerRight.y - that.upperLeft.y))

  override def toString: String =
    val upperLeft = this.upperLeft
    val lowerRight = this.lowerRight
    upperLeft.y until lowerRight.y map { y =>
      upperLeft.x until lowerRight.x map {
        x => this(Pos(x, y)).toString
      } mkString ""
    } mkString "\n"

end Grid

object Grid:

  def hstrip[A](default: A, values: Seq[A]): Grid[A] =
    Grid(default, HashMap.from(0 until values.length map { Pos(_, 0) } zip values))

  def vstrip[A](default: A, values: Seq[A]): Grid[A] =
    hstrip(default, values).rotateRight

  def empty[A](default: A): Grid[A] =
    Grid(default)

  def singleton[A](default: A): Grid[A] =
    Grid(default, HashMap(Pos.Zero -> default))

end Grid
