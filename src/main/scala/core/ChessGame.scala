package core

import core.ChessGame.{Board, blackKing, defaultSetup, optionToString, whiteKing}
import core.Color.{Black, White}

import java.util
import scala.Console.println

case class ChessGame(board: Board = defaultSetup, turn: Color = Color.White) extends MoveValidator {
  val dimensionRow: Int = board.length
  val dimensionCol: Int = board(0).length
  val inCheck : Boolean =
    alliedKingPos
      .fold(false)(alliedKing => getAllPossibleMovesForColor(turn.opposite).contains(alliedKing))

  private def getAllPossibleMovesForColor(color: Color): Set[Coordinate] = {
    val allies = getAllCoordinates.filter(coord => isAlly(coord, color))
    allies.flatMap(ally => getAllPossibleMoves(ally))
  }

  private def whiteKingPos: Option[Coordinate] = find(whiteKing)
  private def blackKingPos: Option[Coordinate] = find(blackKing)

  private def alliedKingPos: Option[Coordinate] = turn match {
    case White => whiteKingPos
    case Black => blackKingPos
  }
  private def enemyKingPos: Option[Coordinate] = turn match {
    case White => blackKingPos
    case Black => whiteKingPos
  }

  def nextTurn(move: Move): Option[ChessGame] = {
    validateMove(move).flatMap {
      validMove =>
        val newBoard = board
        newBoard(validMove.to.row)(validMove.to.col) = board(validMove.from.row)(validMove.from.col)
        newBoard(validMove.from.row)(validMove.from.col) = None
        ChessGame(newBoard, turn.opposite)
    }
  }

  protected def getPiece(coordinate: Coordinate): Option[GamePiece] =
    board(coordinate.row)(coordinate.col)

  def printBoard(): Unit = {
    println("=========================")
    if (inCheck) println(s"$turn, in check")
    println(toASCIIBoard.replace("*", " "))
  }

  def toASCIIBoard: String = {
    val stringBoard = board.map(_.map(optionToString)).reverse
    stringBoard.map(_.mkString(" ")).mkString("\n")
  }

  private def find(searchedPiece: GamePiece): Option[Coordinate] =
    getAllCoordinates.find(coord => getPiece(coord).contains(searchedPiece))

  private def getAllCoordinates: Set[Coordinate] =
    (for {
      i <- 0 until dimensionRow
      j <- 0 until dimensionCol
    } yield Coordinate(i, j, dimensionRow, dimensionCol)).flatten.toSet

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
      if !game.getAllPossibleMovesForColor(game.turn).contains(enemyKing)
    } yield game
  }

  val defaultBoardDimension = 8
  type Board = Array[Array[Option[GamePiece]]]

  val noPiece: Option[GamePiece] = None

  val whitePawn: GamePiece = GamePiece(Piece.Pawn, Color.White)
  val whiteRook: GamePiece = GamePiece(Piece.Rook, Color.White)
  val whiteKnight: GamePiece = GamePiece(Piece.Knight, Color.White)
  val whiteBishop: GamePiece = GamePiece(Piece.Bishop, Color.White)
  val whiteQueen: GamePiece = GamePiece(Piece.Queen, Color.White)
  val whiteKing: GamePiece = GamePiece(Piece.King, Color.White)

  val blackPawn: GamePiece = GamePiece(Piece.Pawn, Color.Black)
  val blackRook: GamePiece = GamePiece(Piece.Rook, Color.Black)
  val blackKnight: GamePiece = GamePiece(Piece.Knight, Color.Black)
  val blackBishop: GamePiece = GamePiece(Piece.Bishop, Color.Black)
  val blackQueen: GamePiece = GamePiece(Piece.Queen, Color.Black)
  val blackKing: GamePiece = GamePiece(Piece.King, Color.Black)

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