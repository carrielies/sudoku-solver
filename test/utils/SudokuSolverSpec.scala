package utils

import models.{OptionBox, Board}
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
      SudokuSolver.boxId(OptionBox(0,0)) must be (0)
      SudokuSolver.boxId(OptionBox(2,0)) must be (0)
      SudokuSolver.boxId(OptionBox(0,2)) must be (0)
      SudokuSolver.boxId(OptionBox(2,2)) must be (0)

      SudokuSolver.boxId(OptionBox(0,3)) must be (1)
      SudokuSolver.boxId(OptionBox(2,3)) must be (1)
      SudokuSolver.boxId(OptionBox(0,5)) must be (1)
      SudokuSolver.boxId(OptionBox(2,5)) must be (1)

      SudokuSolver.boxId(OptionBox(3,0)) must be (3)
      SudokuSolver.boxId(OptionBox(5,0)) must be (3)
      SudokuSolver.boxId(OptionBox(3,2)) must be (3)
      SudokuSolver.boxId(OptionBox(5,2)) must be (3)

      SudokuSolver.boxId(OptionBox(6,6)) must be (8)
      SudokuSolver.boxId(OptionBox(8,6)) must be (8)
      SudokuSolver.boxId(OptionBox(6,8)) must be (8)
      SudokuSolver.boxId(OptionBox(8,8)) must be (8)
    }
  }

  "groupBy" must {
    "successfully group by rows" in {
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
      val result = SudokuSolver.groupBy(optionBoard, (item: OptionBox) => item.x)
      result.size must be (9)
      result(0)(0) must be (optionBoard(0)(0))
      result(0)(1) must be (optionBoard(0)(1))
      result(1)(0) must be (optionBoard(1)(0))
    }

    "successfully group by cols" in {
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
      val result = SudokuSolver.groupBy(optionBoard, (item: OptionBox) => item.y)
      result.size must be (9)
      result(0)(0) must be (optionBoard(0)(0))
      result(0)(1) must be (optionBoard(1)(0))
      result(1)(0) must be (optionBoard(0)(1))
    }
    "successfully group by boxId" in {
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
      val result = SudokuSolver.groupBy(optionBoard, (item: OptionBox) => SudokuSolver.boxId(item))
      result.size must be (9)
      result(0)(0) must be (optionBoard(0)(0))
      result(0)(1) must be (optionBoard(0)(1))
      result(0)(3) must be (optionBoard(1)(0))

      result(1)(0) must be (optionBoard(0)(3))
      result(1)(1) must be (optionBoard(0)(4))
      result(1)(3) must be (optionBoard(1)(3))
    }

  }


  "removePairsFromGroup" must {
    "remove the correct options when we have a single pair" in {
      val optionValues = IndexedSeq(
        OptionBox(0,0, None, IndexedSeq(2,3)),
        OptionBox(0,1, Some(1)),
        OptionBox(0,2, None, IndexedSeq(2,3)),
        OptionBox(0,3, None, IndexedSeq(4,5)),
        OptionBox(0,4, None, IndexedSeq(2,3,4,5,6,7,8))
      )
      val result = SudokuSolver.removePairsFromGroup(optionValues)

      result.size must be (5)
      result(0).options must be (IndexedSeq(2,3))
      result(2).options must be (IndexedSeq(2,3))
      result(3).options must be (IndexedSeq(4,5))
      result(4).options must be (IndexedSeq(4,5,6,7,8))
    }


    "remove the correct options when we have a two pairs" in {
      val optionValues = IndexedSeq(
        OptionBox(0,0, None, IndexedSeq(2,3)),
        OptionBox(0,1, Some(1)),
        OptionBox(0,2, None, IndexedSeq(2,3)),
        OptionBox(0,3, None, IndexedSeq(4,5)),
        OptionBox(0,4, None, IndexedSeq(2,3,4,5,6,7,8)),
        OptionBox(0,5, None, IndexedSeq(4,5))
      )
      val result = SudokuSolver.removePairsFromGroup(optionValues)

      result.size must be (6)
      result(0).options must be (IndexedSeq(2,3))
      result(2).options must be (IndexedSeq(2,3))
      result(3).options must be (IndexedSeq(4,5))
      result(4).options must be (IndexedSeq(6,7,8))
      result(5).options must be (IndexedSeq(4,5))
    }
  }

  "setWhereAnOptionIsUsedOnce" must {
    "set a value if an option is only in one Box" in {
      val optionValues = IndexedSeq(
        OptionBox(0,0, None, IndexedSeq(2)),
        OptionBox(0,1, Some(1), IndexedSeq.empty[Int]),
        OptionBox(0,2, None, IndexedSeq(3)),
        OptionBox(0,3, None, IndexedSeq(4,5,6,7,8)),
        OptionBox(0,4, None, IndexedSeq(4,5,6,7,8))
      )
      val result = SudokuSolver.setWhereAnOptionIsUsedOnce(optionValues)

      result.size must be (5)
      result(0).value must be (Some(2))
      result(2).value must be (Some(3))
      result(3).options must be (IndexedSeq(4,5,6,7,8))
      result(4).options must be (IndexedSeq(4,5,6,7,8))
    }

  }

  "removeTrips" must {
    "remove trips from other boxes" in {
      // if we have three option boxes that only have a combination of 2,4,8, then we can remove these options from the others in the group
      // i.e. in the list below item 1,7,8 only have a combination of 2,4,8. So we can remove these from item 0
      val optionValues = IndexedSeq(
        OptionBox(0,0, None, IndexedSeq(4,7,8,9)),
        OptionBox(0,1, None, IndexedSeq(4,8)),
        OptionBox(0,2, Some(6), IndexedSeq.empty[Int]),
        OptionBox(0,3, None, IndexedSeq(1,2,9)),
        OptionBox(0,4, Some(5), IndexedSeq.empty[Int]),
        OptionBox(0,5, None, IndexedSeq(1,2,7)),
        OptionBox(0,6, Some(3), IndexedSeq.empty[Int]),
        OptionBox(0,7, None, IndexedSeq(2,4)),
        OptionBox(0,8, None, IndexedSeq(2,8))
      )
      val result = SudokuSolver.removeTripOptionsFromGroup(optionValues)

      result.size must be (9)
      result(0).options must be (IndexedSeq(7,9))
      result(1).options must be (IndexedSeq(4,8))
      result(3).options must be (IndexedSeq(1,9))
      result(5).options must be (IndexedSeq(1,7))
      result(7).options must be (IndexedSeq(2,4))
      result(8).options must be (IndexedSeq(2,8))
    }

  }
}
