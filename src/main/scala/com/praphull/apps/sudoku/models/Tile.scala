package com.praphull.apps.sudoku.models

case class Tile(x: Int, y: Int) {
  @inline final def isValid(x: Int, y: Int): Boolean = x >= 0 && x < 9 && y >= 0 && y < 9

  require(isValid(x, y), s"Invalid tile $x, $y")

  def gridSequence: Int = {
    //This is a particularly strange equation, and it is so because of precision loss needed here
    (3 * (x / 3)) + (y / 3)
  }

  def nextTile: Option[Tile] = {
    val (dx, dy) = if (y == 8) (x + 1, 0) else (x, y + 1)
    if (isValid(dx, dy)) Some(Tile(dx, dy)) else None
  }

}

object Tile {
  def apply(index: Int): Tile = Tile(index / 9, index % 9)
}
