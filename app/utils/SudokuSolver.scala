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

  def setValueFromOption(value: OptionBox) : OptionBox = {
    if (value.options.size == 1) {
      value.copy(value = Some(value.options(0)), options = IndexedSeq.empty[Int])
    } else {
      value
    }
  }
  def filterByOptionGroups(options: IndexedSeq[OptionBox], groupSize: Int): IndexedSeq[OptionBox] = {
    def filterValueByOptionGroup(value: OptionBox, optionGroups: SolutionBoard): OptionBox = {
      if (optionGroups.contains(value.options)) {
        value
      } else {
        setValueFromOption(value.copy(options=value.options.diff(optionGroups.flatten)))
      }
    }
    val optionGroups = findOptionGroups(options, groupSize)
    options.map(filterValueByOptionGroup(_, optionGroups))
  }

  def findOptionGroups(options: IndexedSeq[OptionBox], groupSize: Int): SolutionBoard = {
    def countOccurrences(values: IndexedSeq[Int], allOptions: SolutionBoard) = {
      allOptions.filter{checkOption => checkOption == values}.size
    }

    def findDuplicates(allOptions: SolutionBoard) = {
      allOptions.filter{
        optionList => countOccurrences(optionList, allOptions) == groupSize
      }.distinct
    }

    val filteredOptions = options.filter(item => !item.value.isDefined && item.options.size == groupSize).map(_.options)
    findDuplicates(filteredOptions)
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
        setValueFromOption(item.copy(options = item.options.diff(valuesInSameGroup)))
      }
      case Some(value) => item
    }
  }


  def removeOptions(optionBoard: OptionsBoard): OptionsBoard = {
    val values = findSetValues(optionBoard)
    val updatedBoard = optionBoard.map(_.map(useValuesToRemoveOptions(_, values)))
    val groupsRemoved = removeOptionGroups(updatedBoard)

    val newValues = findSetValues(groupsRemoved)
    if (values.size == newValues.size) {
      groupsRemoved
    } else {
      removeOptions(groupsRemoved)
    }
  }


  def removeOptionGroups(optionBoard: OptionsBoard): OptionsBoard = {

    //Remove groups from rows
    optionBoard.map{ row =>
      val filteredRow = filterByOptionGroups(row, 2)
      filterByOptionGroups(filteredRow, 3)
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
