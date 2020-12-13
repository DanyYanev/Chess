package core

import core.ChessGame.{Board, blackKing, defaultSetup, getAllPossibleMovesForColor, optionToString, whiteKing}
import core.Color.{Black, White}
import core.MoveValidator.{getAllPossibleMoves, validateMove}

import java.util
import scala.Console.println

case class ChessGame(board: Board = defaultSetup, turn: Color = Color.White) {
  val dimensionRow = board.length
  val dimensionCol = board(0).length
  val inCheck : Boolean =
    alliedKingPos
      .fold(false)(alliedKing => getAllPossibleMovesForColor(this, turn.opposite).contains(alliedKing))

  def whiteKingPos: Option[Coordinate] = find(whiteKing)
  def blackKingPos: Option[Coordinate] = find(blackKing)

  def alliedKingPos: Option[Coordinate] = turn match {
    case White => whiteKingPos
    case Black => blackKingPos
  }
  def enemyKingPos: Option[Coordinate] = turn match {
    case White => blackKingPos
    case Black => whiteKingPos
  }

  def nextTurn(move: Move): Option[ChessGame] = {
    validateMove(this, move).flatMap {
      validMove =>
        val newBoard = board
        newBoard(validMove.to.row)(validMove.to.col) = board(validMove.from.row)(validMove.from.col)
        newBoard(validMove.from.row)(validMove.from.col) = None
        ChessGame(newBoard, turn.opposite)
    }
  }

  def printBoard = {
    println("=========================")
    if (inCheck) println(s"$turn, in check")
    println(toASCIIBoard.replace("*", " "))
  }

  def toASCIIBoard: String = {
    val stringBoard = board.map(_.map(optionToString)).reverse
    stringBoard.map(_.mkString(" ")).mkString("\n")
  }

  def find(searchedPiece: GamePiece): Option[Coordinate] =
    getAllCoordinates.find(coord => getPiece(coord).contains(searchedPiece))

  def getAllCoordinates: Set[Coordinate] =
    (for {
      i <- 0 until dimensionRow
      j <- 0 until dimensionCol
    } yield Coordinate(i, j, dimensionRow, dimensionCol)).flatten.toSet

  def getPiece(coordinate: Coordinate): Option[GamePiece] =
    board(coordinate.row)(coordinate.col)

  def canEqual(other: Any): Boolean = other.isInstanceOf[ChessGame]

  //Scala's Array equals method cannot handle multidimensional arrays properly
  override def equals(other: Any): Boolean = other match {
    case that: ChessGame =>
      (that canEqual this) &&
        inCheck == that.inCheck &&
        sameBoard(that.board) &&
        turn == that.turn
    case _ => false
  }

  private def sameBoard(otherBoard: Board): Boolean = {
    util.Arrays.deepEquals(board.toArray, otherBoard.toArray)
  }
}

object ChessGame {

  def apply(board: Board = defaultSetup, turn: Color = Color.White): Option[ChessGame] = {
    val game = new ChessGame(board, turn)

    for {
      enemyKing <- game.enemyKingPos
      if !getAllPossibleMovesForColor(game, game.turn).contains(enemyKing)
    } yield game
  }

  def getAllPossibleMovesForColor(game: ChessGame, color: Color): Set[Coordinate] = {
    val allies = game.getAllCoordinates.filter(coord => isAlly(game, coord, color))
    allies.flatMap(ally => getAllPossibleMoves(game, ally))
  }

  def isAlly(game: ChessGame, coordinate: Coordinate, alliedColor: Color): Boolean = game.getPiece(coordinate) match {
    case Some(GamePiece(_, color)) if color == alliedColor => true
    case _ => false
  }

  val defaultBoardDimension = 8
  type Board = Array[Array[Option[GamePiece]]]

  val noPiece = None

  val whitePawn = GamePiece(Piece.Pawn, Color.White)
  val whiteRook = GamePiece(Piece.Rook, Color.White)
  val whiteKnight = GamePiece(Piece.Knight, Color.White)
  val whiteBishop = GamePiece(Piece.Bishop, Color.White)
  val whiteQueen = GamePiece(Piece.Queen, Color.White)
  val whiteKing = GamePiece(Piece.King, Color.White)

  val blackPawn = GamePiece(Piece.Pawn, Color.Black)
  val blackRook = GamePiece(Piece.Rook, Color.Black)
  val blackKnight = GamePiece(Piece.Knight, Color.Black)
  val blackBishop = GamePiece(Piece.Bishop, Color.Black)
  val blackQueen = GamePiece(Piece.Queen, Color.Black)
  val blackKing = GamePiece(Piece.King, Color.Black)

  def defaultSetup: Board = Array(
    Array(whiteRook, whiteKnight, whiteBishop, whiteQueen, whiteKing, whiteBishop, whiteKnight, whiteRook),
    Array(whitePawn, whitePawn, whitePawn, whitePawn, whitePawn, whitePawn, whitePawn, whitePawn),
    Array(noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece),
    Array(noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece),
    Array(noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece),
    Array(noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece),
    Array(blackPawn, blackPawn, blackPawn, blackPawn, blackPawn, blackPawn, blackPawn, blackPawn),
    Array(blackRook, blackKnight, blackBishop, blackQueen, blackKing, blackBishop, blackKnight, blackRook),
  )

  def optionToString(gp: Option[GamePiece]): String = gp match {
    case Some(gp) => gp.toString
    case None => "*"
  }

  implicit def GamePieceToOption(gp : GamePiece) : Option[GamePiece] = Some(gp)
}