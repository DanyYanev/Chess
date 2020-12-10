package core

import core.ChessGame._
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class MoveValidatorSpec extends AnyWordSpec with should.Matchers {

  "MoveValidator" when {
    "validateForward" should {
      "empty white" in {
        val board: Board = Array(
          Array(whiteRook),
          Array(noPiece),
          Array(noPiece),
          Array(noPiece),
          Array(noPiece),
        )
        val game = new ChessGame(board)

        val expectedValidMoves = Set(
          Coordinate(1, 0),
          Coordinate(2, 0),
          Coordinate(3, 0),
          Coordinate(4, 0),
        ).flatten

        MoveValidator.getValidMovesForward(game, Coordinate(0, 0).get) shouldBe expectedValidMoves
      }

      "empty black" in {
        val board: Board = Array(
          Array(noPiece),
          Array(noPiece),
          Array(noPiece),
          Array(noPiece),
          Array(blackRook),
        )
        val game = new ChessGame(board)

        val expectedValidMoves = Set(
          Coordinate(0, 0),
          Coordinate(1, 0),
          Coordinate(2, 0),
          Coordinate(3, 0),
        ).flatten

        MoveValidator.getValidMovesForward(game, Coordinate(4, 0).get) shouldBe expectedValidMoves
      }

      "white enemy piece in the way" in {
        val board: Board = Array(
          Array(noPiece),
          Array(noPiece),
          Array(whitePawn),
          Array(noPiece),
          Array(blackRook),
        )
        val game = new ChessGame(board)

        val expectedValidMoves = Set(
          Coordinate(2, 0),
          Coordinate(3, 0),
        ).flatten

        MoveValidator.getValidMovesForward(game, Coordinate(4, 0).get) shouldBe expectedValidMoves
      }

      "black ally piece in the way" in {
        val board: Board = Array(
          Array(whiteRook),
          Array(noPiece),
          Array(blackPawn),
          Array(noPiece),
          Array(blackRook),
        )
        val game = new ChessGame(board)

        val expectedValidMoves = Set(
          Coordinate(3, 0),
        ).flatten

        MoveValidator.getValidMovesForward(game, Coordinate(4, 0).get) shouldBe expectedValidMoves
      }

      "white ally piece in the way" in {
        val board: Board = Array(
          Array(whiteRook),
          Array(noPiece),
          Array(whitePawn),
          Array(noPiece),
          Array(blackRook),
        )
        val game = new ChessGame(board)

        val expectedValidMoves = Set(
          Coordinate(1, 0),
        ).flatten

        MoveValidator.getValidMovesForward(game, Coordinate(0, 0).get) shouldBe expectedValidMoves
      }
    }

    "validateLeft" should {
      "empty white" in {
        val board: Board = Array(
          Array(noPiece, noPiece, noPiece, whiteRook),
        )
        val game = new ChessGame(board)

        val expectedValidMoves = Set(
          Coordinate(0, 0),
          Coordinate(0, 1),
          Coordinate(0, 2),
        ).flatten

        MoveValidator.getValidMovesLeft(game, Coordinate(0, 3).get) shouldBe expectedValidMoves
      }

      "pawn blocks white" in {
        val board: Board = Array(
          Array(noPiece, blackPawn, noPiece, whiteRook),
        )
        val game = new ChessGame(board)

        val expectedValidMoves = Set(
          Coordinate(0, 1),
          Coordinate(0, 2),
        ).flatten

        MoveValidator.getValidMovesLeft(game, Coordinate(0, 3).get) shouldBe expectedValidMoves
      }
    }

    "validateRight" should {
      "empty white" in {
        val board: Board = Array(
          Array(whiteRook, noPiece, noPiece, noPiece),
        )
        val game = new ChessGame(board)

        val expectedValidMoves = Set(
          Coordinate(0, 1),
          Coordinate(0, 2),
          Coordinate(0, 3),
        ).flatten

        MoveValidator.getValidMovesRight(game, Coordinate(0, 0).get) shouldBe expectedValidMoves
      }

      "pawn blocks white" in {
        val board: Board = Array(
          Array(whiteRook, noPiece, blackPawn, noPiece),
        )
        val game = new ChessGame(board)

        val expectedValidMoves = Set(
          Coordinate(0, 1),
          Coordinate(0, 2),
        ).flatten

        MoveValidator.getValidMovesRight(game, Coordinate(0, 0).get) shouldBe expectedValidMoves
      }
    }
  }
}
