package core

import core.ChessGame.{Board, defaultBoardDimension, defaultSetup, optionToString}

case class ChessGame(board: Board = defaultSetup, turn: Color = Color.White) {
  val dimensionCol = board.length
  val dimensionRow = board(0).length
  val whiteKing: Coordinate = Coordinate(0, 4).get
  val blackKing: Coordinate = Coordinate(7, 4).get

  def nextTurn(move: Move): ChessGame = {
    this
  }

  def printBoard = {
    println(toASCIIBoard.replace("*", " "))
  }

  def toASCIIBoard: String = {
    val stringBoard = board.map(_.map(optionToString))
    stringBoard.map(_.mkString(" ")).mkString("\n")
  }

  def getPiece(coordinate: Coordinate): Option[GamePiece] =
    board(coordinate.row)(coordinate.col)
}

object ChessGame {
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

  val defaultSetup: Board = Array(
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