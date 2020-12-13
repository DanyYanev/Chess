package core

import core.ChessGame.defaultBoardDimension
import core.Color.{Black, White}
import core.Direction._
import core.Piece._

import scala.math._

trait MoveValidator {
  this: ChessGame =>
  //Returns Some if move is valid, None otherwise
  def validateMove(game: ChessGame, move: Move): Option[Move] = {
    for {
      startPiece <- game.getPiece(move.from)
      if move.from != move.to
      if isValidStart(startPiece, game.turn)
      if isValidTarget(game, move.to)
      if hasValidPath(game, move)
    } yield move
  }

  def hasValidPath(game: ChessGame, move: Move): Boolean = getAllPossibleMoves(game, move.from).contains(move.to)

  def getAllPossibleMoves(game: ChessGame, pieceCoord: Coordinate): Set[Coordinate] = {
    val startPiece = game.getPiece(pieceCoord).get
    startPiece match {
      case GamePiece(King, _) =>
          Direction.values.flatMap(dir => getValidMovesInDir(game, pieceCoord, dir, 1)).toSet

      case GamePiece(Queen, _) =>
          Direction.values.flatMap(dir => getValidMovesInDir(game, pieceCoord, dir)).toSet

      case GamePiece(Rook, _) =>
          getValidMovesDown(game, pieceCoord) ++
            getValidMovesUp(game, pieceCoord) ++
            getValidMovesLeft(game, pieceCoord) ++
            getValidMovesRight(game, pieceCoord)

      case GamePiece(Bishop, _) =>
          getValidMovesUpRight(game, pieceCoord) ++
            getValidMovesUpLeft(game, pieceCoord) ++
            getValidMovesDownRight(game, pieceCoord) ++
            getValidMovesDownLeft(game, pieceCoord)

      case GamePiece(Pawn, color) =>
        val maxMovesForward =
          if (isPawnAtStartPos(pieceCoord, color)) 2 else 1

        getValidMovesInDir(game, pieceCoord, getForwardDir(color), maxMovesForward) ++
          getPawnAttackMoves(game, pieceCoord)

      case GamePiece(Knight, _) =>
        val res1 = for {
          rowOffset <- List(-2, 2)
          colOffset <- List(-1, 1)
        } yield Coordinate(pieceCoord.row + rowOffset, pieceCoord.col + colOffset, game.dimensionRow, game.dimensionCol)
        val res2 = for {
          rowOffset <- List(-1, 1)
          colOffset <- List(-2, 2)
        } yield Coordinate(pieceCoord.row + rowOffset, pieceCoord.col + colOffset, game.dimensionRow, game.dimensionCol)

        (res1 ++ res2).flatten.filter(coord => isNotAlly(game.getPiece(coord), startPiece.color)).toSet
    }
  }

  def getPawnAttackMoves(game: ChessGame, startCoord: Coordinate): Set[Coordinate] = {
    val startPiece = game.getPiece(startCoord).get
    val possibleAttackCoord = getForwardDir(startPiece.color) match {
      case Up =>
        getValidMovesUpRight(game, startCoord, 1) ++
        getValidMovesUpLeft(game, startCoord, 1)
      case Down =>
        getValidMovesDownRight(game, startCoord, 1) ++
        getValidMovesDownLeft(game, startCoord, 1)
    }

    possibleAttackCoord.filter(coord => isEnemy(game.getPiece(coord), startPiece.color.opposite))
  }

  def getForwardDir(color: Color): Direction =
    color match {
      case Black => Up
      case White => Down
    }

  def getValidMovesUp(game: ChessGame, coord: Coordinate, maxMoves: Int = defaultBoardDimension): Set[Coordinate] = getValidMovesInDir(game, coord, Direction.Up, maxMoves)
  def getValidMovesDown(game: ChessGame, coord: Coordinate, maxMoves: Int = defaultBoardDimension): Set[Coordinate] = getValidMovesInDir(game, coord, Direction.Down, maxMoves)
  def getValidMovesRight(game: ChessGame, coord: Coordinate, maxMoves: Int = defaultBoardDimension): Set[Coordinate] = getValidMovesInDir(game, coord, Direction.Right, maxMoves)
  def getValidMovesLeft(game: ChessGame, coord: Coordinate, maxMoves: Int = defaultBoardDimension): Set[Coordinate] = getValidMovesInDir(game, coord, Direction.Left, maxMoves)

  def getValidMovesUpRight(game: ChessGame, coord: Coordinate, maxMoves: Int = defaultBoardDimension): Set[Coordinate] = getValidMovesInDir(game, coord, Direction.UpRight, maxMoves)
  def getValidMovesUpLeft(game: ChessGame, coord: Coordinate, maxMoves: Int = defaultBoardDimension): Set[Coordinate] = getValidMovesInDir(game, coord, Direction.UpLeft, maxMoves)
  def getValidMovesDownRight(game: ChessGame, coord: Coordinate, maxMoves: Int = defaultBoardDimension): Set[Coordinate] = getValidMovesInDir(game, coord, Direction.DownRight, maxMoves)
  def getValidMovesDownLeft(game: ChessGame, coord: Coordinate, maxMoves: Int = defaultBoardDimension): Set[Coordinate] = getValidMovesInDir(game, coord, Direction.DownLeft, maxMoves)

  //TODO Fix names and general refactoring of this garbage code
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
        val maxDistance = min(maxMoves, game.dimensionCol - 1 - coord.col)
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

  def isNotAlly(targetPiece: Option[GamePiece], allyColor: Color): Boolean = targetPiece match {
    case Some(GamePiece(_, color)) if color == allyColor => false
    case _ => true
  }

  def isValidStart(gamePiece: GamePiece, turn: Color): Boolean = gamePiece.color == turn

  def isValidTarget(game: ChessGame, pieceCoord: Coordinate): Boolean = game.getPiece(pieceCoord) match {
    case None => true
    case Some(GamePiece(_, color)) if game.turn.opposite == color => true
    case _ => false
  }

  private def isPawnAtStartPos(coord: Coordinate, color: Color): Boolean = {
    val whiteStartRow = 1
    val blackStartRow = defaultBoardDimension - 1 - 1
    color match {
      case White => coord.row == whiteStartRow
      case Black => coord.row == blackStartRow
    }
  }
}