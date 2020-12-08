package core

import enumeratum._

case class GamePiece(piece: Piece, color: Color){
  override def toString: String = color match {
    case Color.White => piece.alias.toUpperCase
    case Color.Black => piece.alias.toLowerCase
  }
}

sealed abstract class Piece(val alias: String) extends EnumEntry

object Piece extends Enum[Piece] {
  val values = findValues

  case object Empty  extends Piece("*")
  case object Pawn   extends Piece("P")
  case object Rook   extends Piece("R")
  case object Knight extends Piece("N")
  case object Bishop extends Piece("B")
  case object Queen  extends Piece("Q")
  case object King   extends Piece("K")
}

sealed trait Color extends EnumEntry

object Color extends Enum[Color] {
  val values = findValues

  case object White extends Color
  case object Black extends Color
}