package com.praphull.apps.sudoku.backend

import com.praphull.apps.sudoku.models.Board.TileMap
import com.praphull.apps.sudoku.models.{Board, Tile}

import scala.util.{Failure, Success, Try}

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

  //Invalid board
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

  val board7: String =
    """2,,,,,,,7,,
      |4,,8,3,,6,,,9,
      |,6,,,,,,8,,
      |1,,,,6,8,,,,
      |,,,1,,3,,,,
      |,,,2,5,,,,1,
      |,8,,,,,,2,,
      |3,,,8,,7,9,,4,
      |,4,,,,,,,6""".stripMargin

  def doSolve(tile: Tile, tiles: TileMap): (Boolean, TileMap) = {
    //Check whether tile is already filled, if so, just proceed with next tile until board is solved
    if (!Board.isTileFilled(tile, tiles)) {
      val possibleValues = Board.possibleValuesAt(tile, tiles)
      //Get all valid values for current tile and iterate through them
      possibleValues.foldLeft((false, tiles)) { case ((isAlreadySolved, currentTiles), possibleValue) =>
        //Whatever value is totally suited, short circuit after that
        if (isAlreadySolved) (isAlreadySolved, currentTiles) else {
          val updated = Board.update(tile, possibleValue, tiles)
          //Check if current value keeps the board valid
          if (Board.isValid(tile, updated)) {
            //If value suits, proceed to next tile
            tile.nextTile.fold((Board.isValid(updated), updated)) { nextTile =>
              doSolve(nextTile, updated)
            }
          } else {
            //If value doesn't suit, return false so that next valid value will be tried at this position
            (false, tiles)
          }
        }
      }
    } else {
      //If no next tile remains, return
      tile.nextTile.fold((Board.isSolved(tiles), tiles)) { nextTile =>
        //Otherwise solve next tile with current board
        doSolve(nextTile, tiles)
      }
    }
  }

  def solve(board: Board): Unit = {
    board.print()
    if (board.isValid) {
      val start = System.nanoTime
      val (solved, solution) = doSolve(Tile(0, 0), board.tiles)
      val elapsed = (System.nanoTime - start) / 1000000
      if (solved) {
        println(s"Solved in ${elapsed / 1000} seconds ($elapsed ms)")
        Board(solution).print()
      } else {
        println(s"Could not solve")
      }
    } else {
      println(s"Given board is invalid")
    }
  }

  val boards = Map(
    1 -> board1,
    2 -> board2,
    3 -> board3,
    4 -> board4,
    5 -> board5,
    6 -> board6,
    7 -> board7,
  )

  def main(args: Array[String]): Unit = {
    val boardStringOpt = if (args.length == 1) {
      if (args(0).forall(_.isDigit)) boards.get(args(0).toInt) else Some(args(0))
    } else Some(boards(1))
    boardStringOpt.fold {
      val bk = boards.keys
      println(s"Incorrect argument. Either pass a board as comma separated or send a board number between ${bk.min}-${bk.max}")
    } { boardString =>
      Try(Board.fromString(boardString)) match {
        case Failure(exception) => println(s"Failed to create board: ${exception.getMessage}")
        case Success(board) => solve(board)
      }
    }
  }
}
