package com.praphull.apps.sudoku.backend

import com.praphull.apps.sudoku.models.Board.TileMap
import com.praphull.apps.sudoku.models.{Board, Tile}

object Solver {
  val board1: String =
    """,,2,,,1,,,9,
      |,,8,,,2,4,,,
      |3,,,,6,,,2,7,
      |2,,,,,,,,,
      |,1,,7,,4,,9,,
      |,,,,,,,,8,
      |7,5,,,2,,,,1,
      |,,4,9,,,7,,,
      |9,,,1,,,5,,,""".stripMargin

  val board2: String =
    """2,1,,6,,,,5,,
      |,,3,,4,,,,,
      |,,,,1,2,,3,9,
      |,3,5,,,,1,,6,
      |6,,8,1,,9,5,,2,
      |7,,1,,,,3,9,,
      |4,8,,5,2,,,,,
      |,,,,8,,6,,,
      |,7,,,,1,,8,5""".stripMargin

  val board3: String =
    """1,,4,9,,3,8,,,
      |,2,,4,,8,5,,,
      |6,,,,,,,4,3,
      |4,,,2,,,9,,,
      |,3,,7,,9,,1,,
      |,,7,,,1,,,2,
      |2,1,,,,,,,8,
      |,,9,8,,6,,7,,
      |,,6,1,,4,2,,9""".stripMargin
  val board4: String =
    """,,5,6,,,,,,
      |,6,,,5,1,8,,9,
      |,,8,4,,7,6,,,
      |6,7,9,,,,,,,
      |3,,4,,,,5,,6,
      |,,,,,,9,7,4,
      |,,3,8,,9,7,,,
      |9,,7,1,4,,,6,,
      |,,,,,5,4,,,""".stripMargin

  val board5: String =
    """,,8,,4,3,,,,
      |,,,,,,9,7,,
      |,,2,7,,,5,4,3,
      |,9,4,,,2,7,,5,
      |7,,,8,1,9,,,2,
      |2,,6,4,,,8,3,,
      |3,8,3,,,5,1,,,
      |,5,9,,,,,,,
      |,,,6,9,,3,,,""".stripMargin

  val board6: String =
    """8,,,,,,,,,
      |,4,1,,9,,,5,,
      |3,,9,,,5,,,7,
      |,,,9,,,,,8,
      |,1,4,,,,7,3,,
      |7,,,,,3,,,,
      |5,,,6,,,1,,2,
      |,3,,,2,,4,6,,
      |,,,,,,,,5,""".stripMargin

  def doSolve(tile: Tile, tiles: TileMap): (Boolean, TileMap) = {
    // def log(s: String): Unit = println(s"[${tile.x}, ${tile.y}] $s")
    //Check whether tile is already filled, if so, just proceed with next tile until board is solved
    if (!Board.isTileFilled(tile, tiles)) {
      val validValues = Board.validValuesAt(tile, tiles)
      //log(s"Trying valid values: ${validValues.mkString(",")}")
      //Get all valid values for current tile and iterate through them
      validValues.foldLeft((false, tiles)) { case ((r, currentTiles), value) =>
        //Whatever value is totally suited, short circuit after that
        if (r) (r, currentTiles) else {
          val updated = Board.withTile(tile, value, tiles)
          //Check if current value keeps the board valid
          if (Board.isValid(tile, updated)) {
            //    log(s"Value $value works")
            //If value suits, proceed to next tile
            tile.nextTile.fold((Board.isValid(updated), updated)) { nextTile =>
              doSolve(nextTile, updated)
            }
          } else {
            //  log(s"Value $value doesn't work")
            //If value doesn't suit, return false so that next valid value will be tried at this position
            (false, tiles)
            /*tile.nextTile.fold((false, currentTiles)) { nextTile =>
              doSolve(nextTile, currentTiles)
            }*/
          }
        }
      }
      /*if (gotSolved && Board.isSolved(updatedTiles)) (gotSolved, updatedTiles)
      else {
        (false, updatedTiles)
        //log(s"Proceeding to check next tile. No solution found for current tile")
        //If board isn't solved, proceed to next tiles (shouldn't happen)
        //tile.nextTile.fold((false, updatedTiles)) { nextTile =>
        //doSolve(nextTile, updatedTiles)
        //}
      }*/
    } else {
      //log(s"Tile already filled")
      //If no next tile remains, return
      tile.nextTile.fold((Board.isSolved(tiles), tiles)) { nextTile =>
        //Other solve next tile with current board
        doSolve(nextTile, tiles)
      }
    }
  }

  def solve(board: Board): Unit = {
    board.print()
    //val test = Board.validValuesAt(Tile(0,4),board.tiles)
    //println(s"Valid values at 0,4: ${test.mkString(",")} \n-----")

    val (r, res) = doSolve(Tile(0, 0), board.tiles)
    if (r) {
      println(s"Solved.")
      Board(res).print()
    } else {
      println(s"Could not solve")
    }
  }

  val boards = Map(
    1 -> board1,
    2 -> board2,
    3 -> board3,
    4 -> board4,
    5 -> board5,
    6 -> board6,
  )

  def main(args: Array[String]): Unit = {
    val toSolve = if (args.length == 1) args(0).toInt else 1
    solve(Board.fromString(boards(toSolve)))
  }
}
