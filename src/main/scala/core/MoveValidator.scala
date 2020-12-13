package core

import core.ChessGame.defaultBoardDimension
import core.Color.{Black, White}
import core.Direction._
import core.Piece._

trait MoveValidator {
  this: ChessGame =>
  //Returns Some if move is valid, None otherwise
  def validateMove(move: Move): Option[Move] = {
    for {
      startPiece <- getPiece(move.from)
      if move.from != move.to
      if isValidStart(startPiece, turn)
      if isValidTarget(move.to)
      if hasValidPath(move)
    } yield move
  }

  private def isValidStart(gamePiece: GamePiece, turn: Color): Boolean = gamePiece.color == turn

  private def isValidTarget(pieceCoord: Coordinate): Boolean = getPiece(pieceCoord) match {
    case None => true
    case Some(GamePiece(_, color)) if turn.opposite == color => true
    case _ => false
  }

  private def hasValidPath(move: Move): Boolean = getAllPossibleMoves(move.from).contains(move.to)

  def getAllPossibleMoves(pieceCoord: Coordinate): Set[Coordinate] = {
    val startPiece = getPiece(pieceCoord).get
    startPiece match {
      case GamePiece(King, _) =>
        Direction.values.flatMap(dir => getValidMovesInDir(pieceCoord, dir, 1)).toSet

      case GamePiece(Queen, _) =>
        Direction.values.flatMap(dir => getValidMovesInDir(pieceCoord, dir)).toSet

      case GamePiece(Rook, _) =>
        getValidMovesDown(pieceCoord) ++
          getValidMovesUp(pieceCoord) ++
          getValidMovesLeft(pieceCoord) ++
          getValidMovesRight(pieceCoord)

      case GamePiece(Bishop, _) =>
        getValidMovesUpRight(pieceCoord) ++
          getValidMovesUpLeft(pieceCoord) ++
          getValidMovesDownRight(pieceCoord) ++
          getValidMovesDownLeft(pieceCoord)

      case GamePiece(Pawn, color) =>
        val maxMovesForward =
          if (isPawnAtStartPos(pieceCoord, color)) 2 else 1

        getValidMovesInDir(pieceCoord, getForwardDir(color), maxMovesForward) ++
          getPawnAttackMoves(pieceCoord)

      case GamePiece(Knight, _) =>
        val res1 = for {
          rowOffset <- List(-2, 2)
          colOffset <- List(-1, 1)
        } yield Coordinate(
          pieceCoord.row + rowOffset,
          pieceCoord.col + colOffset,
          dimensionRow,
          dimensionCol
        )
        val res2 = for {
          rowOffset <- List(-1, 1)
          colOffset <- List(-2, 2)
        } yield Coordinate(
          pieceCoord.row + rowOffset,
          pieceCoord.col + colOffset,
          dimensionRow,
          dimensionCol
        )

        (res1 ++ res2).flatten.filter(coord => isNotAlly(getPiece(coord), startPiece.color)).toSet
    }
  }

  private def isEnemy(targetPiece: Option[GamePiece], enemyColor: Color): Boolean = targetPiece match {
    case Some(GamePiece(_, color)) if color == enemyColor => true
    case _ => false
  }

  private def isNotAlly(targetPiece: Option[GamePiece], allyColor: Color): Boolean = targetPiece match {
    case Some(GamePiece(_, color)) if color == allyColor => false
    case _ => true
  }

  protected def isAlly(coordinate: Coordinate, alliedColor: Color): Boolean = getPiece(coordinate) match {
    case Some(GamePiece(_, color)) if color == alliedColor => true
    case _ => false
  }

  private def getPawnAttackMoves(startCoord: Coordinate): Set[Coordinate] = {
    val startPiece = getPiece(startCoord).get
    val possibleAttackCoord = getForwardDir(startPiece.color) match {
      case Up =>
        getValidMovesUpRight(startCoord, 1) ++
          getValidMovesUpLeft(startCoord, 1)
      case Down =>
        getValidMovesDownRight(startCoord, 1) ++
          getValidMovesDownLeft(startCoord, 1)
    }

    possibleAttackCoord.filter(coord => isEnemy(getPiece(coord), startPiece.color.opposite))
  }

  def getValidMovesUp(startCoord: Coordinate, maxMoves: Int = defaultBoardDimension): Set[Coordinate] = getValidMovesInDir(startCoord, Direction.Up, maxMoves)

  def getValidMovesDown(startCoord: Coordinate, maxMoves: Int = defaultBoardDimension): Set[Coordinate] = getValidMovesInDir(startCoord, Direction.Down, maxMoves)

  def getValidMovesRight(startCoord: Coordinate, maxMoves: Int = defaultBoardDimension): Set[Coordinate] = getValidMovesInDir(startCoord, Direction.Right, maxMoves)

  def getValidMovesLeft(startCoord: Coordinate, maxMoves: Int = defaultBoardDimension): Set[Coordinate] = getValidMovesInDir(startCoord, Direction.Left, maxMoves)

  def getValidMovesUpRight(startCoord: Coordinate, maxMoves: Int = defaultBoardDimension): Set[Coordinate] = getValidMovesInDir(startCoord, Direction.UpRight, maxMoves)

  def getValidMovesUpLeft(startCoord: Coordinate, maxMoves: Int = defaultBoardDimension): Set[Coordinate] = getValidMovesInDir(startCoord, Direction.UpLeft, maxMoves)

  def getValidMovesDownRight(startCoord: Coordinate, maxMoves: Int = defaultBoardDimension): Set[Coordinate] = getValidMovesInDir(startCoord, Direction.DownRight, maxMoves)

  def getValidMovesDownLeft(startCoord: Coordinate, maxMoves: Int = defaultBoardDimension): Set[Coordinate] = getValidMovesInDir(startCoord, Direction.DownLeft, maxMoves)

  private def getForwardDir(color: Color): Direction =
    color match {
      case Black => Up
      case White => Down
    }

  //TODO Fix names and general refactoring of this garbage code
  private def getValidMovesInDir(
    startCoord: Coordinate,
    dir: Direction,
    maxMovesExternal: Int = defaultBoardDimension
  ): Set[Coordinate] = {
    val startPiece = getPiece(startCoord).get
    val startRow = startCoord.row
    val startCol = startCoord.col
    val maxDistance = getMaxDistanceForDirection(startCoord, dir, maxMovesExternal)
    dir match {
      case Up =>
        val iter = Iterator.iterate(startRow - 1, maxDistance)(_ - 1)
        getPiecesWhileEmptyOrFirstEnemy(iter, startPiece, (row: Int) => Coordinate(row, startCoord.col))
      case Down =>
        val iter = Iterator.iterate(startRow + 1, maxDistance)(_ + 1)
        getPiecesWhileEmptyOrFirstEnemy(iter, startPiece, (row: Int) => Coordinate(row, startCoord.col))
      case Left =>
        val iter = Iterator.iterate(startCol - 1, maxDistance)(_ - 1)
        getPiecesWhileEmptyOrFirstEnemy(iter, startPiece, (col: Int) => Coordinate(startCoord.row, col))
      case Right =>
        val iter = Iterator.iterate(startCol + 1, maxDistance)(_ + 1)
        getPiecesWhileEmptyOrFirstEnemy(iter, startPiece, (col: Int) => Coordinate(startCoord.row, col))
      case DownRight =>
        val iter = Iterator.iterate(1, maxDistance)(_ + 1)
        getPiecesWhileEmptyOrFirstEnemy(iter, startPiece, (offset: Int) => Coordinate(startCoord.row + offset, startCoord.col + offset))
      case DownLeft =>
        val iter = Iterator.iterate(1, maxDistance)(_ + 1)
        getPiecesWhileEmptyOrFirstEnemy(iter, startPiece, (offset: Int) => Coordinate(startCoord.row + offset, startCoord.col - offset))
      case UpLeft =>
        val iter = Iterator.iterate(1, maxDistance)(_ + 1)
        getPiecesWhileEmptyOrFirstEnemy(iter, startPiece, (offset: Int) => Coordinate(startCoord.row - offset, startCoord.col - offset))
      case UpRight =>
        val iter = Iterator.iterate(1, maxDistance)(_ + 1)
        getPiecesWhileEmptyOrFirstEnemy(iter, startPiece, (offset: Int) => Coordinate(startCoord.row - offset, startCoord.col + offset))
    }
  }

  private def getPiecesWhileEmptyOrFirstEnemy(
    iterable: Iterator[Int],
    startPiece: GamePiece,
    buildCoordinate: Int => Option[Coordinate]
  ): Set[Coordinate] = {
    var metEnemy = false
    iterable.takeWhile {
      i =>
        val piece = getPiece(buildCoordinate(i).get)
        if(piece.isEmpty && !metEnemy) true
        else if (!metEnemy && isEnemy(piece, startPiece.color.opposite)) {
          metEnemy = true
          true
        } else false
    }.flatMap(buildCoordinate).toSet
  }

  private def getMaxDistanceForDirection(
    startCoord: Coordinate,
    dir: Direction,
    maxMovesExternal: Int
  ): Int = {
    val startCol = startCoord.col
    val startRow = startCoord.row
    dir match {
      case Up =>
        List(dimensionRow - 1, startRow, maxMovesExternal).min
      case Down =>
        List(dimensionRow - 1, dimensionRow - 1 - startRow, maxMovesExternal).min
      case Left =>
        List(dimensionCol - 1, startCol, maxMovesExternal).min
      case Right =>
        List(dimensionCol - 1, dimensionCol - 1 - startCol, maxMovesExternal).min
      case UpRight =>
        List(dimensionCol - 1 - startCol, startRow, maxMovesExternal).min
      case UpLeft =>
        List(startCol, startRow, maxMovesExternal).min
      case DownRight =>
        List(dimensionCol - 1 - startCol, dimensionRow - 1 - startRow, maxMovesExternal).min
      case DownLeft =>
        List(startCol, dimensionRow - 1 - startRow, maxMovesExternal).min
    }
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