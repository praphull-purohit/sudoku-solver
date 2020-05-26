package com.praphull.apps.sudoku.models

import com.praphull.apps.sudoku.models.Board.TileMap

case class Board(tiles: TileMap) {
  require(tiles.size == 81, s"Invalid board of size ${tiles.size}: $tiles")

  def isTileFilled(tile: Tile): Boolean = Board.isTileFilled(tile, tiles)

  def withTile(tile: Tile, value: Int): TileMap = Board.withTile(tile, value, tiles)

  def updated(tile: Tile, value: Int): Board = Board.updated(tile, value, tiles)

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

  def isSolved: Boolean = Board.isSolved(tiles)
}

object Board {
  type TileMap = Map[Tile, Option[Int]]

  @inline def isValid(seq: Seq[Int]): Boolean = {
    !seq.groupBy(identity).exists(_._2.length > 1)
    //println(s"isValid failed for grouped: $g")
  }

  def tilesInGrid(seq: Int): Seq[Tile] = {
    val (x, y) = (seq / 3, seq % 3)
    val res = (for (i <- 3 * x until 3 * (x + 1)) yield
      for (j <- 3 * y until 3 * (y + 1)) yield {
        Tile(i, j)
      }).flatten
    res
  }

  def tilesInRow(row: Int): Seq[Tile] = {
    val res = (0 until 9).map(col => Tile(row, col))
    res
  }

  def tilesInCol(col: Int): Seq[Tile] = (0 until 9).map(row => Tile(row, col))

  def validateGrid(tiles: TileMap, seq: Int): Boolean = {
    val res = isValid(tilesInGrid(seq).flatMap(tiles))
    //if (!r) println(s"validateGrid failed for grid# $seq ($x, $y) (${(3 * x)} to ${3 * (x + 1)}) & (${(3 * y)} to ${3 * (y + 1)})")
    res
  }

  def validateRow(tiles: TileMap, row: Int): Boolean = {
    val res = isValid(tilesInRow(row).flatMap(tiles))
    //if (!res) println(s"validateRow failed for row# $row")
    res
  }

  def validateColumn(tiles: TileMap, col: Int): Boolean = {
    val res = isValid(tilesInCol(col).flatMap(tiles))
    //if (!res) println(s"validateColumn failed for col# $col")
    res
  }

  def isValid(tiles: TileMap): Boolean = (0 until 9).forall { i =>
    val res = validateGrid(tiles, i) && validateRow(tiles, i) && validateColumn(tiles, i)
    res
  }

  def isValid(tile: Tile, tiles: TileMap): Boolean = {
    val res = validateGrid(tiles, tile.gridSequence) && isValid(tiles)
    res
  }

  def isSolved(tiles: TileMap): Boolean = {
    val res = tiles.forall(_._2.isDefined) && isValid(tiles)
    res
  }

  def fromString(s: String): Board = {
    val cleaned = s.replaceAll("\n", "")
    val elements = cleaned.foldLeft((false, 0, List.empty[(Tile, Option[Int])])) {
      case ((skipComma, index, list), char) =>
        if (char == ',') {
          if (skipComma)
            (false, index, list)
          else
            (false, index + 1, (Tile(index), None) :: list)
        } else
          (true, index + 1, (Tile(index), Some(char - '0')) :: list)
    }._3
    Board(elements.toMap)
    /*Board(s.replaceAll("\n", "").split(",", -1)
      .zipWithIndex.map { case (c, i) =>
      (Tile(i), if (c.isEmpty) None else Some(c.toInt))
    }.toMap)*/
  }

  @inline final def isTileFilled(tile: Tile, tiles: TileMap): Boolean = tiles(tile).isDefined

  def validValuesAt(tile: Tile, tiles: TileMap): List[Int] = {
    /*val diffWith = {
      val g = tilesInGrid(tile.gridSequence)
      println(s"grid: ${g.map(x => tiles(x)).mkString(",")}")
      val r = tilesInRow(tile.x)
      println(s"row: ${r.map(x => tiles(x)).mkString(",")}")
      val c = tilesInCol(tile.y)
      println(s"col: ${c.map(x => tiles(x)).mkString(",")}")
      val d = (g ++ r ++ c).distinct
      val f = d.filterNot(_ == tile)
      val res = f.flatMap(x => tiles(x)).distinct
      println(s"res: ${res.mkString(",")}")
      res
    }*/
    val diffWith = (tilesInGrid(tile.gridSequence) ++ tilesInRow(tile.x) ++ tilesInCol(tile.y)).distinct
      .filterNot(_ == tile)
      .flatMap(x => tiles(x)).distinct

    (1 to 9).diff(diffWith).toList.sorted
  }

  def withTile(tile: Tile, value: Int, tiles: TileMap): TileMap = tiles.updated(tile, Some(value))

  def updated(tile: Tile, value: Int, tiles: TileMap): Board = Board(withTile(tile, value, tiles))
}
