package core

import core.ChessGame.defaultBoardDimension
import core.Color.{Black, White}
import core.Direction.{Down, DownLeft, DownRight, Up, UpLeft, UpRight}
import core.Piece.{King, Pawn, Rook}

import math._

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
        val possibleMoves =
          getValidMovesDown(game, move.from) ++
            getValidMovesUp(game, move.from) ++
            getValidMovesLeft(game, move.from) ++
            getValidMovesRight(game, move.from)

        possibleMoves.contains(move.to)
      case _ => false
    }
  }


  def getValidMovesForward(game: ChessGame, coord: Coordinate): Set[Coordinate] = {
    val startPiece = game.getPiece(coord).get
    startPiece.color match {
      case White =>
        getValidMovesUp(game, coord)
      case Black =>
        getValidMovesDown(game, coord)
    }
  }

  def getValidMovesUp(game: ChessGame, coord: Coordinate, maxMoves: Int = defaultBoardDimension): Set[Coordinate] = getValidMovesInDir(game, coord, Direction.Up, maxMoves)
  def getValidMovesDown(game: ChessGame, coord: Coordinate, maxMoves: Int = defaultBoardDimension): Set[Coordinate] = getValidMovesInDir(game, coord, Direction.Down, maxMoves)
  def getValidMovesRight(game: ChessGame, coord: Coordinate, maxMoves: Int = defaultBoardDimension): Set[Coordinate] = getValidMovesInDir(game, coord, Direction.Right, maxMoves)
  def getValidMovesLeft(game: ChessGame, coord: Coordinate, maxMoves: Int = defaultBoardDimension): Set[Coordinate] = getValidMovesInDir(game, coord, Direction.Left, maxMoves)

  def getValidMovesUpRight(game: ChessGame, coord: Coordinate, maxMoves: Int = defaultBoardDimension): Set[Coordinate] = getValidMovesInDir(game, coord, Direction.UpRight, maxMoves)
  def getValidMovesUpLeft(game: ChessGame, coord: Coordinate, maxMoves: Int = defaultBoardDimension): Set[Coordinate] = getValidMovesInDir(game, coord, Direction.UpLeft, maxMoves)
  def getValidMovesDownRight(game: ChessGame, coord: Coordinate, maxMoves: Int = defaultBoardDimension): Set[Coordinate] = getValidMovesInDir(game, coord, Direction.DownRight, maxMoves)
  def getValidMovesDownLeft(game: ChessGame, coord: Coordinate, maxMoves: Int = defaultBoardDimension): Set[Coordinate] = getValidMovesInDir(game, coord, Direction.DownLeft, maxMoves)

  //TODO Fix names and genaral refactoring of this garbage code
  def getValidMovesInDir(game: ChessGame, coord: Coordinate, dir: Direction, maxMoves1: Int = defaultBoardDimension): Set[Coordinate] = {
    val startPiece = game.getPiece(coord).get
    val startRow = coord.row
    val startCol = coord.col
    var metEnemy = false
    dir match {
      case Up =>
        val maxMoves = min(game.dimensionRow - 1, maxMoves1)
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
          }.flatMap(validRow => Coordinate(validRow, coord.col)).toSet
      case Down =>
        val maxMoves = min(game.dimensionRow - 1, maxMoves1)
        val maxDistance = min(maxMoves, game.dimensionRow - 1 - coord.row)
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
          }.flatMap(validRow => Coordinate(validRow, coord.col)).toSet
      case Direction.Left =>
        val maxMoves = min(game.dimensionCol - 1, maxMoves1)
        val maxDistance = min(maxMoves, coord.col)
        Iterator.iterate(startCol - 1, maxDistance)(_ - 1)
          .takeWhile {
            col =>
              val piece = game.getPiece(Coordinate(coord.row, col).get)
              if(piece.isEmpty && !metEnemy)
                true
              else if (!metEnemy && isEnemy(piece, startPiece.color.opposite)) {
                metEnemy = true
                true
              } else
                false
          }.flatMap(validCol => Coordinate(coord.row, validCol)).toSet
      case Direction.Right =>
        val maxMoves = min(game.dimensionCol - 1, maxMoves1)
        val maxDistance = min(maxMoves, game.dimensionCol - 1 - coord.row)
        Iterator.iterate(startCol + 1, maxDistance)(_ + 1)
          .takeWhile {
            col =>
              val piece = game.getPiece(Coordinate(coord.row, col).get)
              if(piece.isEmpty && !metEnemy)
                true
              else if (!metEnemy && isEnemy(piece, startPiece.color.opposite)) {
                metEnemy = true
                true
              } else
                false
          }.flatMap(validCol => Coordinate(coord.row, validCol)).toSet
      case DownRight =>
        val maxMovesByCol = game.dimensionCol - 1 - startCol
        val maxMovesByRow = game.dimensionRow - 1 - startRow
        val maxMoves2 = min(maxMovesByCol, maxMovesByRow)
        val maxDistance = min(maxMoves1, maxMoves2)

        Iterator.iterate(1, maxDistance)(_ + 1)
          .takeWhile {
            offset =>
              val newCoord = Coordinate(startRow + offset, startCol + offset).get
              val piece = game.getPiece(newCoord)
              if(piece.isEmpty && !metEnemy)
                true
              else if (!metEnemy && isEnemy(piece, startPiece.color.opposite)) {
                metEnemy = true
                true
              } else
                false
          }.flatMap(validOffset => Coordinate(coord.row + validOffset, coord.col + validOffset)).toSet
      case DownLeft =>
        val maxMovesByCol = startCol
        val maxMovesByRow = game.dimensionRow - 1 - startRow
        val maxMoves2 = min(maxMovesByCol, maxMovesByRow)
        val maxDistance = min(maxMoves1, maxMoves2)
        Iterator.iterate(1, maxDistance)(_ + 1)
          .takeWhile {
            offset =>
              val newCoord = Coordinate(startRow + offset, startCol - offset).get
              val piece = game.getPiece(newCoord)
              if(piece.isEmpty && !metEnemy)
                true
              else if (!metEnemy && isEnemy(piece, startPiece.color.opposite)) {
                metEnemy = true
                true
              } else
                false
          }.flatMap(validOffset => Coordinate(coord.row + validOffset, coord.col - validOffset)).toSet
      case UpLeft =>
        //Todo figure out naming
        val maxMovesByCol = startCol
        val maxMovesByRow = startRow
        val maxMoves2 = min(maxMovesByCol, maxMovesByRow)
        val maxDistance = min(maxMoves1, maxMoves2)

        Iterator.iterate(1, maxDistance)(_ + 1)
          .takeWhile {
            offset =>
              val newCoord = Coordinate(startRow - offset, startCol - offset).get
              val piece = game.getPiece(newCoord)
              if(piece.isEmpty && !metEnemy)
                true
              else if (!metEnemy && isEnemy(piece, startPiece.color.opposite)) {
                metEnemy = true
                true
              } else
                false
          }.flatMap(validOffset => Coordinate(coord.row - validOffset, coord.col - validOffset)).toSet
      case UpRight =>
        val maxMovesByCol = game.dimensionCol - 1 - startCol
        val maxMovesByRow = startRow
        val maxMoves2 = min(maxMovesByCol, maxMovesByRow)
        val maxDistance = min(maxMoves1, maxMoves2)

        Iterator.iterate(1, maxDistance)(_ + 1)
          .takeWhile {
            offset =>
              val newCoord = Coordinate(startRow - offset, startCol + offset).get
              val piece = game.getPiece(newCoord)
              if(piece.isEmpty && !metEnemy)
                true
              else if (!metEnemy && isEnemy(piece, startPiece.color.opposite)) {
                metEnemy = true
                true
              } else
                false
          }.flatMap(validOffset => Coordinate(coord.row - validOffset, coord.col + validOffset)).toSet
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