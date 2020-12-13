package core

import core.ChessGame.defaultBoardDimension

case class Move(from: Coordinate, to: Coordinate)

case class Coordinate(row: Int, col: Int)

object Coordinate {
  def apply(row: Int, col: Int): Option[Coordinate] = apply(row, col, defaultBoardDimension, defaultBoardDimension)

  def apply(row: Int, col: Int, dimRow: Int, dimCol: Int): Option[Coordinate] =
    for {
      validRow <- validateDimention(row, dimRow)
      validCol <- validateDimention(col, dimCol)
    } yield new Coordinate(validRow, validCol)

  def validateDimention(i: Int, dim: Int): Option[Int] =
    if(i >= 0 && i < dim) Some(i) else None
}

case class CoordinateDTO(file: Int, rank: Int){
  def toCoordinate: Option[Coordinate] = Coordinate(defaultBoardDimension - file - 1, rank)
}
