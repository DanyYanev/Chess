package core

import org.scalatest.matchers.must.Matchers.convertToAnyMustWrapper
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec
import core.ChessGame.{Board, GamePieceToOption, blackBishop, blackKing, blackKnight, blackPawn, blackQueen, blackRook, noPiece, whiteBishop, whiteKing, whiteKnight, whitePawn, whiteQueen, whiteRook}
import core.Color.{Black, White}

class ChessGameSpec extends AnyWordSpec with should.Matchers {

  "ChessGame" when {
    "printASCII" should {
      "print board as ASCII" in {
        val game = new ChessGame()
        val expectedASCII =
          """r n b q k b n r
            |p p p p p p p p
            |* * * * * * * *
            |* * * * * * * *
            |* * * * * * * *
            |* * * * * * * *
            |P P P P P P P P
            |R N B Q K B N R""".stripMargin

        game.toASCIIBoard mustBe expectedASCII
      }
    }

    "nextMove" should {
      "return Some[ChessGame] for valid move" in {
        val game = new ChessGame()
        val expectedBoard: Board = Array(
          Array(whiteRook, whiteKnight, whiteBishop, whiteQueen, whiteKing, whiteBishop, whiteKnight, whiteRook),
          Array(whitePawn, noPiece,     whitePawn,   whitePawn,  whitePawn, whitePawn,   whitePawn,   whitePawn),
          Array(noPiece,   noPiece,     noPiece,     noPiece,    noPiece,   noPiece,     noPiece,     noPiece),
          Array(noPiece,   whitePawn,   noPiece,     noPiece,    noPiece,   noPiece,     noPiece,     noPiece),
          Array(noPiece,   noPiece,     noPiece,     noPiece,    noPiece,   noPiece,     noPiece,     noPiece),
          Array(noPiece,   noPiece,     noPiece,     noPiece,    noPiece,   noPiece,     noPiece,     noPiece),
          Array(blackPawn, blackPawn,   blackPawn,   blackPawn,  blackPawn, blackPawn,   blackPawn,   blackPawn),
          Array(blackRook, blackKnight, blackBishop, blackQueen, blackKing, blackBishop, blackKnight, blackRook),
        )
        val expectedGame = new ChessGame(expectedBoard, Color.Black)

        game.nextTurn(Move(Coordinate(1, 1).get, Coordinate(3, 1).get)) shouldBe Some(expectedGame)
      }

      "return None for invalid move wrong color" in {
        val game = new ChessGame()
        val blackPawnForward = Move(Coordinate(6, 1).get, Coordinate(4, 1).get)

        game.nextTurn(blackPawnForward) shouldBe None
      }

      "return None for invalid move" in {
        val game = new ChessGame()
        val blackPawnForward = Move(Coordinate(5, 5).get, Coordinate(5, 5).get)

        game.nextTurn(blackPawnForward) shouldBe None
      }
    }

    "apply" should {
      "return None if enemy king is inCheck at start" in {
        val board: Board = Array(
          Array(blackKing),
          Array(whiteRook),
          Array(whiteKing)
        )
        val game = ChessGame(board, White)

        game shouldBe None
      }
    }

    "inCheck" should {
      "be true if king of color `turn` is under attack" in {
        val board: Board = Array(
          Array(blackKing),
          Array(whiteRook),
        )
        val game = new ChessGame(board, Black)

        game.inCheck shouldBe true
      }
    }
  }
}