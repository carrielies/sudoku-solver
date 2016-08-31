package utils

import org.scalatestplus.play._

class SudokuSolverSpec extends PlaySpec {

  "SudokuSolver" must {
    "solve" must {
      "return an empty list when passed None" in {
        SudokuSolver.solve(None).isDefined must be (false)
      }
    }
  }

}
