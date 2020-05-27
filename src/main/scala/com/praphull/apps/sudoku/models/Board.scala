package com.praphull.apps.sudoku.models

import com.praphull.apps.sudoku.models.Board.TileMap

case class Board(tiles: TileMap) {
  require(tiles.size == 81, s"Invalid board of size ${tiles.size}: $tiles")

  def print(): Unit = {
    println("_________________")
    (0 until 9).foreach { x =>
      (0 until 9).foreach { y =>
        printf(s"${tiles(Tile(x, y)).fold("_")(v => s"$v")} ")
      }
      println("")
    }
    println("_________________")
  }

  def isValid: Boolean = Board.isValid(tiles)
}

object Board {
  type TileMap = Map[Tile, Option[Int]]

  /** Given a sequence of values, check if there's any duplicate */
  @inline def areValuesValid(values: Seq[Int]): Boolean = {
    values.length == values.distinct.length
    //!values.groupBy(identity).exists(_._2.length > 1)
  }

  /** Get all tiles in a given grid */
  def tilesInGrid(seq: Int): Seq[Tile] = {
    val (x, y) = (seq / 3, seq % 3)
    (for (i <- 3 * x until 3 * (x + 1)) yield
      for (j <- 3 * y until 3 * (y + 1)) yield {
        Tile(i, j)
      }).flatten

  }

  /** Get all tiles in a given row */
  def tilesInRow(row: Int): Seq[Tile] = (0 until 9).map(col => Tile(row, col))

  /** Get all tiles in a given column */
  def tilesInCol(col: Int): Seq[Tile] = (0 until 9).map(row => Tile(row, col))

  /** Validates grid */
  def validateGrid(seq: Int, tiles: TileMap): Boolean = areValuesValid(tilesInGrid(seq).flatMap(tiles))

  /** Validates row */
  def validateRow(row: Int, tiles: TileMap): Boolean = areValuesValid(tilesInRow(row).flatMap(tiles))

  /** Validates column */
  def validateColumn(col: Int, tiles: TileMap): Boolean = areValuesValid(tilesInCol(col).flatMap(tiles))

  /** Validates all grids, rows and columns */
  def isValid(tiles: TileMap): Boolean = (0 until 9).forall { i =>
    validateGrid(i, tiles) && validateRow(i, tiles) && validateColumn(i, tiles)
  }

  /** An optimization to first check for the validity of grid in which the tile being updated lies */
  def isValid(tile: Tile, tiles: TileMap): Boolean = validateGrid(tile.gridSequence, tiles) && isValid(tiles)

  def isSolved(tiles: TileMap): Boolean = tiles.forall(_._2.isDefined) && isValid(tiles)

  /** Construct a board from comma separated sequence of numbers */
  def fromString(s: String): Board = {
    val (_, _, elements) = s.replaceAll("\n", "").foldLeft((false, 0, List.empty[(Tile, Option[Int])])) {
      case ((skipComma, index, list), char) =>
        if (char == ',') {
          if (skipComma)
            (false, index, list)
          else
            (false, index + 1, (Tile(index), None) :: list)
        } else {
          val value = char - '0'
          if (value < 1 || value > 9) throw new Exception(s"Invalid tile value: $value")
          (true, index + 1, (Tile(index), Some(value)) :: list)
        }
    }
    Board(elements.toMap)
  }

  @inline final def isTileFilled(tile: Tile, tiles: TileMap): Boolean = tiles(tile).isDefined

  /** Returns list of possible values at given tile */
  def possibleValuesAt(tile: Tile, tiles: TileMap): List[Int] = {
    val diffWith = (tilesInGrid(tile.gridSequence) ++ tilesInRow(tile.x) ++ tilesInCol(tile.y)).distinct
      .filterNot(_ == tile)
      .flatMap(x => tiles(x)).distinct

    (1 to 9).diff(diffWith).toList.sorted
  }

  def update(tile: Tile, value: Int, tiles: TileMap): TileMap = tiles.updated(tile, Some(value))
}
