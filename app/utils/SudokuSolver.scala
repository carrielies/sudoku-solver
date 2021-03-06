package utils

import models.{OptionBox, Board}

object SudokuSolver {

  type SolutionBoard = IndexedSeq[IndexedSeq[Int]]
  type OptionsBoard = IndexedSeq[IndexedSeq[OptionBox]]

  def solve(board: Option[SolutionBoard]) = {
    board.map{ foundBoard =>
      solveBoard(convertToOptionsBoard(foundBoard))
    }
  }

  def convertToOptionsBoard(board: SolutionBoard): OptionsBoard = {
    def convertIntToOptionBox(x:Int, y: Int, item: Int) = {
      if (item == 0) {
        OptionBox(x, y, isCalculated = true)
      } else {
        OptionBox(x, y, Some(item), IndexedSeq.empty[Int], isCalculated = false)
      }
    }

    board.zipWithIndex.map{rowTuple =>
      rowTuple._1.zipWithIndex.map{ colTuple =>
        convertIntToOptionBox(rowTuple._2,colTuple._2,colTuple._1)
      }
    }
  }




  def solveBoard(optionBoard: OptionsBoard): OptionsBoard = {
    val rowUpdated = solvePerGroup(optionBoard)
    val colUpdated = solvePerGroup(groupByCols(rowUpdated))
    val boxUpdated = solvePerGroup(groupByBoxes(colUpdated))
    val boardUpdated = convertToNormal(boxUpdated)

    //Keep updating until nothing changes
    if (boardUpdated == optionBoard) {
      boardUpdated
    } else {
      solveBoard(boardUpdated)
    }
  }

  def solvePerGroup(optionBoard: OptionsBoard): OptionsBoard = {
    optionBoard.map{group =>
      setWhereAnOptionIsUsedOnce(
        removeGroupedOptions(createOptionTripGroups, 3,
          removeGroupedOptions(createOptionPairGroups, 2,
              removeOptionsFromGroup(group))
          )
      )
    }
  }


  def setValueFromOptions(value: OptionBox) : OptionBox = {
    if (value.options.size == 1) {
      value.copy(value = Some(value.options(0)), options = IndexedSeq.empty[Int],reason = "One Option")
    } else {
      value
    }
  }

  def removeOptionsFromGroup(group: IndexedSeq[OptionBox]) : IndexedSeq[OptionBox] = {
    val values = group.map(_.value).flatten
    val updatedGroup = group.map(item => setValueFromOptions(item.copy(options = item.options.diff(values))))
    val newValues = updatedGroup.map(_.value).flatten
    if (newValues.size > values.size)
      removeOptionsFromGroup(updatedGroup)
    else
      updatedGroup
  }


  def setWhereAnOptionIsUsedOnce(group: IndexedSeq[OptionBox]) : IndexedSeq[OptionBox] = {
    def setValueFromSingleOption(item: OptionBox, optionsWeCanSet : IndexedSeq[Int]) = {
      val foundInList = item.options.find(opt => optionsWeCanSet.contains(opt))
      foundInList match {
        case Some(x) => item.copy(value = Some(x), options = IndexedSeq.empty[Int], reason = "Option Once")
        case _ => item
      }
    }
    val optionsToBeSet = group.map(_.options).flatten.distinct
    val optionsWeCanSet = optionsToBeSet.filter { option =>
      group.filter(opt => opt.options.contains(option)).size == 1
    }
    //Now set the value for any items that has a value in the optionsWeCanSet
    removeOptionsFromGroup(
      group.map { item =>
        item.value match {
          case Some(x) => item
          case None => setValueFromSingleOption(item, optionsWeCanSet)
        }
      }
    )
  }

  def createOptionPairGroups(group: IndexedSeq[OptionBox]): SolutionBoard = {
    val values = group.map(_.value).flatten
    if (values.size <= 7) {

      val optionsLeft = group.map(_.options).flatten.distinct
      for {
        x <- optionsLeft
        y <- optionsLeft
        if (x < y)
      } yield IndexedSeq(x, y)

    } else {
      IndexedSeq(IndexedSeq.empty[Int])
    }
  }

  def createOptionTripGroups(group: IndexedSeq[OptionBox]): SolutionBoard = {
    val values = group.map(_.value).flatten
    if (values.size <= 6) {

      val optionsLeft = group.map(_.options).flatten.distinct
      for {
        x <- optionsLeft
        y <- optionsLeft
        z <- optionsLeft
        if (x < y && y < z)
      } yield IndexedSeq(x, y, z)

    } else {
      IndexedSeq(IndexedSeq.empty[Int])
    }
  }

  def removeGroupedOptions(groupFunction: IndexedSeq[OptionBox] => SolutionBoard,
                           groupSize: Int,
                           group: IndexedSeq[OptionBox]) : IndexedSeq[OptionBox] = {
    def containsOnlyTheseOptions(item : OptionBox, options: IndexedSeq[Int]): Boolean ={
      (!item.value.isDefined && item.options.diff(options).size == 0)
    }
    def countOccurrences(checkOptions: IndexedSeq[Int], groupToCheck: IndexedSeq[OptionBox]) = {
      groupToCheck.filter{containsOnlyTheseOptions(_, checkOptions)}.size
    }
    def filterOptions(item: OptionBox, optionsGroupsToCheck: IndexedSeq[IndexedSeq[Int]]) = {
      def optionsToRemove(optionsToCheck: IndexedSeq[Int]) = {
        if (item.value.isDefined || containsOnlyTheseOptions(item, optionsToCheck)) {
          IndexedSeq.empty[Int]
        } else {
          optionsToCheck
        }
      }

      val unwantedOptions = optionsGroupsToCheck.map(optionsToRemove(_)).flatten
      setValueFromOptions(item.copy(options = item.options.diff(unwantedOptions)))
    }

    val optionTrips = groupFunction(group)

    val tripsToRemove = optionTrips.filter{item =>
      countOccurrences(item, group) == groupSize
    }
    removeOptionsFromGroup(
      group.map{ filterOptions(_, tripsToRemove)}
    )

  }

  def boxId(item: OptionBox):Int = {
    (item.x / 3) * 3 + (item.y / 3)
  }

  def groupBy(optionBoard: OptionsBoard, grouper: OptionBox => Int) : OptionsBoard = {
    val res = optionBoard.flatten.groupBy(grouper(_)).toIndexedSeq
    res.sortBy(_._1).map(_._2)
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

  def isValid(optionBoard: Option[OptionsBoard]): Boolean = {
    def isGroupValid(group: OptionsBoard) = {
      val correctValue = (1 to 9).toIndexedSeq
      val results = group.map{subgroup =>
        val values = subgroup.map(_.value).flatten.distinct
        (values.size == 9 && values.diff(correctValue).size == 0)
      }
      !results.contains(false)
    }
    optionBoard match {
      case Some(x) => {
        val rowValid = isGroupValid(x)
        val colValid = isGroupValid(groupByCols(x))
        val squareValid = isGroupValid(groupByBoxes(x))
        (rowValid && colValid && squareValid)
      }
      case None => false
    }
  }

  
  def groupByCols(optionBoard: OptionsBoard) : OptionsBoard = {
    groupBy(optionBoard, (item: OptionBox) => item.y)
  }

  def groupByBoxes(optionBoard: OptionsBoard) : OptionsBoard = {
    groupBy(optionBoard, (item: OptionBox) => boxId(item))
  }
  def convertToNormal(optionBoard: OptionsBoard) : OptionsBoard = {
    groupBy(optionBoard, (item: OptionBox) => item.x)
  }
}
