package utils

import models.Board

object SudokuSolver {

  type SolutionBoard = IndexedSeq[IndexedSeq[Int]]

  def solve(board: Option[Board]) = {
    board.getOrElse(new Board(99,IndexedSeq.empty)).board
  }


  def bruteForce(board: SolutionBoard) = {
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

    bruteForceSolve(board)

  }
}
