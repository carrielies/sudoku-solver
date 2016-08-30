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
    getSudoku(id) match {
      case Some(foundBoard) => {
        val solution = utils.SudokuSolver.bruteForce(foundBoard.board)
        Ok(views.html.solved(solution))
      }
      case None => Ok(views.html.solved(None))
    }


  }


  private def getSudoku(id: Int) = {
    sudokus.find(board => board.id == id)
  }

  private def sudokus() = {
    //0s denote empty cells
    List(new Board(0, $(
      $(1, 0, 0, 0, 0, 7, 0, 9, 0),
      $(0, 3, 0, 0, 2, 0, 0, 0, 8),
      $(0, 0, 9, 6, 0, 0, 5, 0, 0),
      $(0, 0, 5, 3, 0, 0, 9, 0, 0),
      $(0, 1, 0, 0, 8, 0, 0, 0, 2),
      $(6, 0, 0, 0, 0, 4, 0, 0, 0),
      $(3, 0, 0, 0, 0, 0, 0, 1, 0),
      $(0, 4, 0, 0, 0, 0, 0, 0, 7),
      $(0, 0, 7, 0, 0, 0, 3, 0, 0)
    )),
    new Board(1, $(
      $(1, 0, 0, 0, 0, 7, 0, 9, 0),
      $(0, 3, 0, 0, 2, 0, 0, 0, 8),
      $(0, 0, 9, 6, 0, 0, 5, 0, 0),
      $(0, 0, 5, 3, 0, 0, 9, 0, 0),
      $(0, 1, 0, 0, 8, 0, 0, 0, 2),
      $(6, 0, 0, 0, 0, 4, 0, 0, 0),
      $(3, 0, 0, 0, 0, 0, 0, 1, 0),
      $(0, 4, 0, 0, 0, 0, 0, 0, 7),
      $(0, 0, 7, 0, 0, 0, 3, 0, 0)
    )))
  }
}
