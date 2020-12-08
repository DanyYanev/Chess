package core

class Coordinate(val file: Char, val rank: Int)

object Coordinate{
  def apply(file: Char, rank: Int): Option[Coordinate] =
    for {
      maybeFile <- validateFile(file)
      maybeRank <- validateRank(rank)
    } yield new Coordinate(maybeFile, maybeRank)

  def validateFile(file: Char): Option[Char] =
    if(file >= 'a' && file <='z') Some(file) else None

  def validateRank(rank: Int): Option[Int] =
    if(rank > 0 && rank <= 8) Some(rank) else None
}

case class Move(from: Coordinate, to: Coordinate)

object MoveValidator {
  //Returns Some if move is valid, None otherwise
  def validateMove(game: ChessGame, move: Move): Option[Move] = {
    None
  }

}