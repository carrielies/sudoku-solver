package utils

import models.{Board, OptionBox}
import org.scalatestplus.play._
import scala.collection.{IndexedSeq => $}

class SudokuSolverSpec extends PlaySpec {

  "solve" must {
    "return an empty list when passed None" in {
      SudokuSolver.solve(None).isDefined must be (false)
    }
  }

  "bruteForce" must {
    "return an empty list when passed None" in {
      SudokuSolver.bruteForce(None).isDefined must be (false)
    }
  }

  "box" must {
    "return the correct value for the x, y co-ordinates" in {
      SudokuSolver.box(0,0) must be (0)
      SudokuSolver.box(2,0) must be (0)
      SudokuSolver.box(0,2) must be (0)
      SudokuSolver.box(2,2) must be (0)

      SudokuSolver.box(0,3) must be (1)
      SudokuSolver.box(2,3) must be (1)
      SudokuSolver.box(0,5) must be (1)
      SudokuSolver.box(2,5) must be (1)

      SudokuSolver.box(3,0) must be (3)
      SudokuSolver.box(5,0) must be (3)
      SudokuSolver.box(3,2) must be (3)
      SudokuSolver.box(5,2) must be (3)

      SudokuSolver.box(6,6) must be (8)
      SudokuSolver.box(8,6) must be (8)
      SudokuSolver.box(6,8) must be (8)
      SudokuSolver.box(8,8) must be (8)
    }
  }

  "findSetValues" must {
    "return the correct values in a list" in {
      val board = $(
        $(1, 0, 0, 0, 0, 7, 0, 9, 0),
        $(0, 3, 0, 0, 2, 0, 0, 0, 8),
        $(0, 0, 9, 6, 0, 0, 5, 0, 0),
        $(0, 0, 5, 3, 0, 0, 9, 0, 0),
        $(0, 1, 0, 0, 8, 0, 0, 0, 2),
        $(6, 0, 0, 0, 0, 4, 0, 0, 0),
        $(3, 0, 0, 0, 0, 0, 0, 1, 0),
        $(0, 4, 0, 0, 0, 0, 0, 0, 7),
        $(0, 0, 7, 0, 0, 0, 3, 0, 0)
      )

      val optionBoard = SudokuSolver.convertToOptionsBoard(board)
      val result = SudokuSolver.findSetValues(optionBoard)
      result.size must be (23)
      result(0).x must be (0)
      result(0).y must be (0)
      result(0).value must be (Some(1))
      result(1).x must be (0)
      result(1).y must be (5)
      result(1).value must be (Some(7))
    }
  }

  "removeOptions" must {
    "remove the correct options" in {
      val board = $(
        $(1, 0, 0, 0, 0, 7, 0, 9, 0)
      )
      val optionBoard = SudokuSolver.convertToOptionsBoard(board)
      val result = SudokuSolver.removeOptions(optionBoard)

      val item1 = result(0)(0)
      val item2 = result(0)(1)
      item1.value must be (Some(1))
      item2.value must be (None)
      item2.options must be (IndexedSeq(2,3,4,5,6,8))
    }
    "set missing values " in {
      val board = $(
        $(1, 2, 3, 4, 0, 6, 7, 8, 9)
      )
      val optionBoard = SudokuSolver.convertToOptionsBoard(board)
      val result = SudokuSolver.removeOptions(optionBoard)

      val item1 = result(0)(0)
      val item4 = result(0)(4)
      item1.value must be (Some(1))
      item4.value must be (Some(5))
    }
  }

  "useValuesToRemoveOptions" must {
    "remove the correct options" in {
      val item = OptionBox(0,0, None)
      val values = IndexedSeq(OptionBox(1,1, Some(1)))

      item.options.size must be (9)
      item.options.diff(IndexedSeq(1,2,3)).size must be (6)


      val result = SudokuSolver.useValuesToRemoveOptions(item, values)
      result.options.size must be (8)

    }
  }
}
