package utils

import models.{OptionBox, Board}

object SudokuSolver {

  type SolutionBoard = IndexedSeq[IndexedSeq[Int]]
  type OptionsBoard = IndexedSeq[IndexedSeq[OptionBox]]

  def solve(board: Option[SolutionBoard]) = {
    board.map{ foundBoard =>
      val optionsBoard = convertToOptionsBoard(foundBoard)

      removeOptions(optionsBoard)
    }
  }

  def convertToOptionsBoard(board: SolutionBoard): OptionsBoard = {
    def convertIntToOptionBox(x:Int, y: Int, item: Int) = {
      if (item == 0) {
        OptionBox(x, y)
      } else {
        OptionBox(x, y, Some(item), IndexedSeq.empty[Int])
      }
    }

    board.zipWithIndex.map{rowTuple =>
      rowTuple._1.zipWithIndex.map{ colTuple =>
        convertIntToOptionBox(rowTuple._2,colTuple._2,colTuple._1)
      }
    }
  }

  def findSetValues(optionBoard: OptionsBoard): IndexedSeq[OptionBox] = {
    val values = optionBoard.map(_.map(optionBox =>
      optionBox.value match {
        case Some(value) => Some(optionBox)
        case _ => None
      }
    ))
    values.flatten.flatten
  }

  def box(x: Int, y: Int) = {
    (x / 3) * 3 + (y / 3)
  }

  def useValuesToRemoveOptions(item: OptionBox, values: IndexedSeq[OptionBox]) = {
    def isSameGroup(item: OptionBox, value: OptionBox) = {
      (item.x == value.x || item.y == value.y || box(item.x, item.y) == box(value.x, value.y))
    }
    item.value match {
      case None => {
        val valuesInSameGroup = values.filter( isSameGroup(item, _)).flatMap(_.value)
        val optionList = item.options.diff(valuesInSameGroup)
        if (optionList.size == 1) {
          item.copy(value = Some(optionList(0)), options = IndexedSeq.empty[Int])
        } else {
          item.copy(options = optionList)
        }
      }
      case Some(value) => item
    }
  }


  def removeOptions(optionBoard: OptionsBoard): OptionsBoard = {
    val values = findSetValues(optionBoard)
    val updatedBoard = optionBoard.map(_.map(useValuesToRemoveOptions(_, values)))
    val newValues = findSetValues(updatedBoard)
    if (values.size == newValues.size) {
      updatedBoard
    } else {
      removeOptions(updatedBoard)
    }
  }




  def bruteForce(board: Option[SolutionBoard]) = {
    val n = 9
    val s = Math.sqrt(n).toInt

    def bruteForceSolve(board: SolutionBoard, cell: Int = 0): Option[SolutionBoard] = (cell%n, cell/n) match {
      case (r, `n`) => Some(board)
      case (r, c) if board(r)(c) > 0 => bruteForceSolve(board, cell + 1)
      case (r, c) =>
        def cells(i: Int) = Seq(board(r)(i), board(i)(c), board(s*(r/s) + i/s)(s*(c/s) + i%s))
        def guess(x: Int) = bruteForceSolve(board.updated(r, board(r).updated(c, x)), cell + 1)
        val used = board.indices flatMap cells
        1 to n diff used collectFirst Function.unlift(guess)
    }
    board.flatMap{ foundBoard =>
      bruteForceSolve(foundBoard)
    }

  }
}
