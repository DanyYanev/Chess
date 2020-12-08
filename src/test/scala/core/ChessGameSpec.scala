package core

import org.scalatest.matchers.must.Matchers.convertToAnyMustWrapper
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec
import core.ChessGame.{Board, GamePieceToOption, blackBishop, blackKing, blackKnight, blackPawn, blackQueen, blackRook, noPiece, whiteBishop, whiteKing, whiteKnight, whitePawn, whiteQueen, whiteRook}

class ChessGameSpec extends AnyWordSpec with should.Matchers {

  "ChessGame" should {
    "print ASCII" in {
      val game = new ChessGame()
      val expectedASCII =
        """R N B Q K B N R
          |P P P P P P P P
          |* * * * * * * *
          |* * * * * * * *
          |* * * * * * * *
          |* * * * * * * *
          |p p p p p p p p
          |r n b q k b n r""".stripMargin

      game.toASCIIBoard mustBe expectedASCII
    }

    "getPiece" in {
      val game = new ChessGame()
      game.getPiece(Coordinate('a', 1).get) mustBe Some(whiteRook)
    }

    "nextMove" in {
      val game = new ChessGame()
      val expectedBoard: Board = Array(
        Array(whiteRook, whiteKnight, whiteBishop, whiteQueen, whiteKing, whiteBishop, whiteKnight, whiteRook),
        Array(whitePawn, whitePawn,   whitePawn,   whitePawn,  whitePawn, whitePawn,   whitePawn,   whitePawn),
        Array(noPiece,   noPiece,     noPiece,     noPiece,    noPiece,   noPiece,     noPiece,     noPiece),
        Array(noPiece,   noPiece,     noPiece,     noPiece,    noPiece,   noPiece,     noPiece,     noPiece),
        Array(noPiece,   blackPawn,   noPiece,     noPiece,    noPiece,   noPiece,     noPiece,     noPiece),
        Array(noPiece,   noPiece,     noPiece,     noPiece,    noPiece,   noPiece,     noPiece,     noPiece),
        Array(blackPawn, noPiece,     blackPawn,   blackPawn,  blackPawn, blackPawn,   blackPawn,   blackPawn),
        Array(blackRook, blackKnight, blackBishop, blackQueen, blackKing, blackBishop, blackKnight, blackRook),
      )
      val expectedGame = new ChessGame(expectedBoard, Color.Black)
      
      game.nextTurn shouldBe expectedGame
    }
  }
}