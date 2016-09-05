package models

case class OptionBox(x: Int, y: Int, value: Option[Int] = None, options: IndexedSeq[Int] = (1 to 9).toIndexedSeq, isCalculated: Boolean = true, reason: String = "")

case class Board(id: Int, board: IndexedSeq[IndexedSeq[Int]])
