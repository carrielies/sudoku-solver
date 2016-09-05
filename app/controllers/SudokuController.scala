package controllers

import models.Board

import scala.collection.{IndexedSeq => $}
import play.api.Play.current
import play.api.mvc._

object SudokuController extends Controller {


  def index = Action {
    Ok(views.html.index(sudokus))
  }

  def bruteForce(id:Int) = Action {
    val solution = utils.SudokuSolver.bruteForce(getSudoku(id).map(_.board))
    Ok(views.html.bruteForceSolved(solution))
  }

  def solve(id:Int) = Action {
    val solution = utils.SudokuSolver.solve(getSudoku(id).map(_.board))
    Ok(views.html.solved(utils.SudokuSolver.isValid(solution), solution))
  }

  private def getSudoku(id: Int) = {
    sudokus.find(board => board.id == id)
  }

  private def sudokus() = {
    //0s denote empty cells
    List(new Board(0, $(
      $(1, 0, 0,  0, 0, 7,  0, 9, 0),
      $(0, 3, 0,  0, 2, 0,  0, 0, 8),
      $(0, 0, 9,  6, 0, 0,  5, 0, 0),
      $(0, 0, 5,  3, 0, 0,  9, 0, 0),
      $(0, 1, 0,  0, 8, 0,  0, 0, 2),
      $(6, 0, 0,  0, 0, 4,  0, 0, 0),
      $(3, 0, 0,  0, 0, 0,  0, 1, 0),
      $(0, 4, 0,  0, 0, 0,  0, 0, 7),
      $(0, 0, 7,  0, 0, 0,  3, 0, 0)
    )),
    new Board(1, $(
      $(0,1,0,  3,0,4,  5,0,0),
      $(5,0,7,  0,1,0,  4,0,0),
      $(0,4,0,  0,7,0,  0,1,9),
      $(4,0,0,  7,0,2,  0,0,8),
      $(0,3,8,  0,0,0,  2,9,0),
      $(2,0,0,  6,0,8,  0,0,7),
      $(9,8,0,  0,2,0,  0,6,0),
      $(0,0,4,  0,8,0,  9,0,5),
      $(0,0,3,  9,0,6,  0,2,0)
    )),
    new Board(2, $(
      $(0,3,0,  0,0,5,  9,0,0),
      $(8,5,0,  0,0,3,  0,0,0),
      $(0,0,0,  7,1,0,  0,0,3),

      $(0,0,8,  0,0,0,  0,2,4),
      $(0,0,5,  0,0,0,  6,0,0),
      $(3,9,0,  0,0,0,  1,0,0),

      $(6,0,0,  0,8,7,  0,0,0),
      $(0,0,0,  6,0,0,  0,4,1),
      $(0,0,1,  2,0,0,  0,7,0)
    )))
  }
}
