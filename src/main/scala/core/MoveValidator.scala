package core

import core.ChessGame.defaultBoardDimension
import core.Color.{Black, White}
import core.Direction.{Down, Up}
import core.Piece.{King, Pawn, Rook}

import math.min
import scala.::
import scala.collection.immutable.Nil.:::

class CoordinateDTO(val file: Char, val rank: Int){
  def toCoordinate: Coordinate = new Coordinate(file - 'a', rank - 1)
}
case class Coordinate(row: Int, col: Int){
  def toCoordinateDTO: CoordinateDTO = new CoordinateDTO((row + 'a').toChar, col + 1)
}

object Coordinate{
  def apply(row: Int, col: Int): Option[Coordinate] =
    for {
      row <- validateDimention(row)
      col <- validateDimention(col)
    } yield new Coordinate(row, col)

  def validateDimention(dim: Int): Option[Int] =
    if(dim >= 0 && dim < defaultBoardDimension) Some(dim) else None
}

object CoordinateDTO{
  def apply(file: Char, rank: Int): Option[CoordinateDTO] =
    for {
      maybeFile <- validateFile(file)
      maybeRank <- validateRank(rank)
    } yield new CoordinateDTO(maybeFile, maybeRank)

  def validateFile(file: Char): Option[Char] =
    if(file >= 'a' && file <='z') Some(file) else None

  def validateRank(rank: Int): Option[Int] =
    if(rank > 0 && rank <= 8) Some(rank) else None
}

case class Move(from: Coordinate, to: Coordinate)

object MoveValidator {

  //Returns Some if move is valid, None otherwise
  def validateMove(game: ChessGame, move: Move): Option[Move] = {
    for {
      startPiece <- game.getPiece(move.from)
      if isValidStart(startPiece, game.turn)
      if isValidTarget(game, move.to)
      if hasValidPath(game, move)
    } yield move
  }

  def hasValidPath(game: ChessGame, move: Move): Boolean = {
    val startPiece = game.getPiece(move.from).get
    val startCoord = move.from
    startPiece match {
      case GamePiece(King, _) =>
        val possibleCoordinates = for {
          i <- startCoord.row - 1 to startCoord.row + 1
          j <- startCoord.col - 1 to startCoord.col + 1
        } yield Coordinate(i, j)
        possibleCoordinates.contains(move.to)
      case GamePiece(Rook, _) =>
        val possibleCoordinates = getValidMovesForward(game, move.from) ++ getValidMovesBackwards(game, move.from)
        possibleCoordinates.contains(move.to)
      case _ => false
    }
  }


  def getValidMovesForward(game: ChessGame, coord: Coordinate): Set[Coordinate] = {
    val startPiece = game.getPiece(coord).get
    startPiece.color match {
      case White =>
        getValidMovesDown(game, coord)
      case Black =>
        getValidMovesUp(game, coord)
    }
  }

  def getValidMovesBackwards(game: ChessGame, coord: Coordinate): Set[Coordinate] = {
    val startPiece = game.getPiece(coord).get
    startPiece.color match {
      case White =>
        getValidMovesUp(game, coord)
      case Black =>
        getValidMovesDown(game, coord)
    }
  }

  def getValidMovesLeft(game: ChessGame, coord: Coordinate): Set[Coordinate] = getValidMovesInDir(game, coord, Direction.Left)

  def getValidMovesRight(game: ChessGame, coord: Coordinate): Set[Coordinate] = getValidMovesInDir(game, coord, Direction.Right)

  def getValidMovesDown(game: ChessGame, coord: Coordinate): Set[Coordinate] = getValidMovesInDir(game, coord, Direction.Down)

  def getValidMovesUp(game: ChessGame, coord: Coordinate): Set[Coordinate] = getValidMovesInDir(game, coord, Direction.Up)

  def getValidMovesInDir(game: ChessGame, coord: Coordinate, dir: Direction): Set[Coordinate] = {
    val startPiece = game.getPiece(coord).get
    val startRow = coord.row
    val maxMoves = game.dimension - 1
    var metEnemy = false
    dir match {
      case Down =>
        val maxDistance = min(maxMoves, game.dimension - 1 - coord.row)
        Iterator.iterate(startRow + 1, maxDistance)(_ + 1)
          .takeWhile {
            row =>
              val piece = game.getPiece(Coordinate(row, coord.col).get)
              if(piece.isEmpty && !metEnemy)
                true
              else if (!metEnemy && isEnemy(piece, startPiece.color.opposite)) {
                metEnemy = true
                true
              } else
                false
          }.flatMap(validRows => Coordinate(validRows, coord.col)).toSet
      case Up =>
        val maxDistance = min(maxMoves, coord.row)
        Iterator.iterate(startRow - 1, maxDistance)(_ - 1)
          .takeWhile {
            row =>
              val piece = game.getPiece(Coordinate(row, coord.col).get)
              if(piece.isEmpty && !metEnemy)
                true
              else if (!metEnemy && isEnemy(piece, startPiece.color.opposite)) {
                metEnemy = true
                true
              } else
                false
          }.flatMap(validRows => Coordinate(validRows, coord.col)).toSet
      case _ => Set()
    }
  }

  def isEnemy(targetPiece: Option[GamePiece], enemyColor: Color): Boolean = targetPiece match {
    case Some(GamePiece(_, color)) if color == enemyColor => true
    case _ => false
  }

  def isValidStart(gamePiece: GamePiece, turn: Color): Boolean = gamePiece.color == turn

  def isValidTarget(game: ChessGame, pieceCoord: Coordinate): Boolean = game.getPiece(pieceCoord) match {
    case None => true
    case Some(GamePiece(_, color)) if game.turn.opposite == color => true
    case _ => false
  }

//  def getAvailableMoves(game: ChessGame, pieceCoord: Coordinate): Option[List[Coordinate]] = {
//    game.getPiece(pieceCoord).map {
//      case GamePiece(King, color) =>
//        val possibleCoordinates = for {
//          i <- pieceCoord.x - 1 to pieceCoord.x + 1
//          j <- pieceCoord.y - 1 to pieceCoord.y + 1
//        } yield Coordinate(i, j)
//
//        possibleCoordinates.flatten.filter(_.equals(pieceCoord)).toList
//
//      case GamePiece(Pawn, color) =>
//        var possibleCoordinates = List[Option[Coordinate]]()
//        if(isPawnAtStartPos(pieceCoord, color))
//          possibleCoordinates :+ Some(pieceCoord.copy(y = pieceCoord.y + 2))
//    }

//  }

  private def isPawnAtStartPos(coord: Coordinate, color: Color): Boolean = {
    val whiteStartRow = 1
    val blackStartRow = defaultBoardDimension - 1
    color match {
      case White => coord.col == whiteStartRow
      case Black => coord.col == blackStartRow
    }
  }
}