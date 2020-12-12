package core

import core.ChessGame._
import core.MoveValidator.{getAllPossibleMoves, getValidMovesInDir}
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class MoveValidatorSpec extends AnyWordSpec with should.Matchers {

  "MoveValidator" when {
    "getAllPossibleMoves" should {
      "queen" in {
        val board: Board = Array(
          Array(noPiece, whitePawn, noPiece, whitePawn),
          Array(noPiece, blackPawn, noPiece, noPiece),
          Array(noPiece, whiteQueen, noPiece, whitePawn),
          Array(noPiece, blackPawn,   noPiece, noPiece),
          Array(noPiece, blackPawn, noPiece, blackPawn),
        )
        val game = new ChessGame(board)
        val startCoord = Coordinate(2, 1).get

        val expectedValidMoves = Set(
          Coordinate(1, 1), Coordinate(1, 0), Coordinate(1, 2),
          Coordinate(2, 0), Coordinate(2, 2),
          Coordinate(3, 0), Coordinate(3, 1), Coordinate(3, 2),
          Coordinate(4, 3),
        ).flatten


        getAllPossibleMoves(game, startCoord) shouldBe expectedValidMoves
      }

      "king" in {
        val board: Board = Array(
          Array(noPiece, whitePawn, noPiece,   noPiece),
          Array(noPiece, blackPawn, whitePawn, noPiece),
          Array(noPiece, whiteKing, noPiece,   whitePawn),
          Array(noPiece, blackPawn, noPiece,   noPiece),
          Array(noPiece, blackPawn, noPiece,   blackPawn),
        )
        val game = new ChessGame(board)
        val startCoord = Coordinate(2, 1).get

        val expectedValidMoves = Set(
          Coordinate(1, 1), Coordinate(1, 0),
          Coordinate(2, 0), Coordinate(2, 2),
          Coordinate(3, 0), Coordinate(3, 1), Coordinate(3, 2),
        ).flatten

        getAllPossibleMoves(game, startCoord) shouldBe expectedValidMoves
      }

      "rook" in {
        val board: Board = Array(
          Array(noPiece, blackPawn, noPiece, noPiece),
          Array(noPiece, whiteRook, noPiece, whitePawn),
          Array(noPiece, blackPawn, noPiece, noPiece),
        )
        val game = new ChessGame(board)
        val startCoord = Coordinate(1, 1).get

        val expectedValidMoves = Set(
          Coordinate(0, 1),
          Coordinate(1, 0), Coordinate(1, 2),
          Coordinate(2, 1)
        ).flatten


        getAllPossibleMoves(game, startCoord) shouldBe expectedValidMoves
      }

      "bishop" in {
        val board: Board = Array(
          Array(noPiece, noPiece,     noPiece, whitePawn),
          Array(noPiece, noPiece,     noPiece, noPiece),
          Array(noPiece, whiteBishop, noPiece, noPiece),
          Array(noPiece, noPiece,     noPiece, noPiece),
          Array(noPiece, noPiece,     noPiece, blackPawn),
        )
        val game = new ChessGame(board)
        val startCoord = Coordinate(2, 1).get

        val expectedValidMoves = Set(
          Coordinate(1, 0), Coordinate(1, 2),
          Coordinate(3, 0), Coordinate(3, 2),
          Coordinate(4, 3),
        ).flatten


        getAllPossibleMoves(game, startCoord) shouldBe expectedValidMoves
      }

      "knight empty" in {
        val board: Board = Array(
          Array(noPiece, noPiece, noPiece,     noPiece, noPiece),
          Array(noPiece, noPiece, noPiece,     noPiece, noPiece),
          Array(noPiece, noPiece, whiteKnight, noPiece, noPiece),
          Array(noPiece, noPiece, noPiece,     noPiece, noPiece),
          Array(noPiece, noPiece, noPiece,     noPiece, noPiece),
        )
        val game = new ChessGame(board)
        val startCoord = Coordinate(2, 2).get

        val expectedValidMoves = Set(
          Coordinate(0, 1), Coordinate(0, 3),
          Coordinate(1, 0), Coordinate(1, 4),
          Coordinate(3, 0), Coordinate(3, 4),
          Coordinate(4, 1), Coordinate(4, 3),
        ).flatten


        getAllPossibleMoves(game, startCoord) shouldBe expectedValidMoves
      }

      "knight" in {
        val board: Board = Array(
          Array(noPiece, blackPawn, noPiece,     whitePawn, noPiece),
          Array(noPiece, noPiece,   noPiece,     noPiece,   noPiece),
          Array(noPiece, noPiece,   whiteKnight, noPiece,   noPiece),
          Array(noPiece, noPiece,   noPiece,     noPiece,   noPiece),
          Array(noPiece, noPiece,   noPiece,     noPiece,   noPiece),
        )
        val game = new ChessGame(board)
        val startCoord = Coordinate(2, 2).get

        val expectedValidMoves = Set(
          Coordinate(0, 1),
          Coordinate(1, 0), Coordinate(1, 4),
          Coordinate(3, 0), Coordinate(3, 4),
          Coordinate(4, 1), Coordinate(4, 3),
        ).flatten


        getAllPossibleMoves(game, startCoord) shouldBe expectedValidMoves
      }

      "pawn" in {
        val board: Board = Array(
          Array(noPiece,   noPiece,   noPiece,   noPiece),
          Array(noPiece,   noPiece,   noPiece,   noPiece),
          Array(noPiece,   whitePawn, noPiece,   noPiece),
          Array(whitePawn, noPiece,   blackPawn, noPiece),
        )
        val game = new ChessGame(board)
        val startCoord = Coordinate(2, 1).get

        val expectedValidMoves = Set(
          Coordinate(3, 1), Coordinate(3, 2),
        ).flatten

        getAllPossibleMoves(game, startCoord) shouldBe expectedValidMoves
      }

      "pawn at starting pos" in {
        val board: Board = Array(
          Array(noPiece,   noPiece,   noPiece,   noPiece),
          Array(noPiece,   whitePawn, noPiece,   noPiece),
          Array(whitePawn, noPiece,   blackPawn, noPiece),
          Array(noPiece,   noPiece,   noPiece,   noPiece),
          Array(noPiece,   noPiece,   noPiece,   noPiece),
        )
        val game = new ChessGame(board)
        val startCoord = Coordinate(1, 1).get

        val expectedValidMoves = Set(
          Coordinate(2, 1), Coordinate(2, 2),
          Coordinate(3, 1)
        ).flatten

        getAllPossibleMoves(game, startCoord) shouldBe expectedValidMoves
      }
    }

    "validateUpRight" should {
      "empty white" in {
        val board: Board = Array(
          Array(whiteBishop, noPiece, noPiece),
          Array(noPiece,     noPiece, noPiece),
          Array(noPiece,     noPiece, noPiece),
          Array(noPiece,     noPiece, noPiece),
          Array(noPiece,     noPiece, noPiece),
        )

        val game = new ChessGame(board)

        val expectedValidMoves = Set(
          Coordinate(1, 1),
          Coordinate(2, 2),
        ).flatten

        MoveValidator.getValidMovesDownRight(game, Coordinate(0, 0).get) shouldBe expectedValidMoves
      }

      "empty white with limit" in {
        val board: Board = Array(
          Array(whiteBishop, noPiece, noPiece),
          Array(noPiece,     noPiece, noPiece),
          Array(noPiece,     noPiece, noPiece),
          Array(noPiece,     noPiece, noPiece),
          Array(noPiece,     noPiece, noPiece),
        )

        val game = new ChessGame(board)

        val expectedValidMoves = Set(
          Coordinate(1, 1),
        ).flatten

        MoveValidator.getValidMovesDownRight(game, Coordinate(0, 0).get, 1) shouldBe expectedValidMoves
      }

      "blocked white" in {
        val board: Board = Array(
          Array(whiteBishop, noPiece, noPiece),
          Array(noPiece,     blackPawn, noPiece),
          Array(noPiece,     noPiece, noPiece),
          Array(noPiece,     noPiece, noPiece),
          Array(noPiece,     noPiece, noPiece),
        )

        val game = new ChessGame(board)

        val expectedValidMoves = Set(
          Coordinate(1, 1),
        ).flatten

        MoveValidator.getValidMovesDownRight(game, Coordinate(0, 0).get) shouldBe expectedValidMoves
      }

      "blocked ally white" in {
        val board: Board = Array(
          Array(whiteBishop, noPiece, noPiece),
          Array(noPiece,     whitePawn, noPiece),
          Array(noPiece,     noPiece, noPiece),
          Array(noPiece,     noPiece, noPiece),
          Array(noPiece,     noPiece, noPiece),
        )

        val game = new ChessGame(board)

        val expectedValidMoves = Set.empty

        MoveValidator.getValidMovesDownRight(game, Coordinate(0, 0).get) shouldBe expectedValidMoves
      }
    }

    "validateUpLeft" should {
      "empty white" in {
        val board: Board = Array(
          Array(noPiece, noPiece, whiteBishop),
          Array(noPiece, noPiece, noPiece),
          Array(noPiece, noPiece, noPiece),
          Array(noPiece, noPiece, noPiece),
          Array(noPiece, noPiece, noPiece),
        )

        val game = new ChessGame(board)

        val expectedValidMoves = Set(
          Coordinate(1, 1),
          Coordinate(2, 0),
        ).flatten

        MoveValidator.getValidMovesDownLeft(game, Coordinate(0, 2).get) shouldBe expectedValidMoves
      }

      "empty white with limit" in {
        val board: Board = Array(
          Array(noPiece, noPiece, whiteBishop),
          Array(noPiece, noPiece, noPiece),
          Array(noPiece, noPiece, noPiece),
          Array(noPiece, noPiece, noPiece),
          Array(noPiece, noPiece, noPiece),
        )

        val game = new ChessGame(board)

        val expectedValidMoves = Set(
          Coordinate(1, 1),
        ).flatten

        MoveValidator.getValidMovesDownLeft(game, Coordinate(0, 2).get, 1) shouldBe expectedValidMoves
      }

      "blocked white" in {
        val board: Board = Array(
          Array(noPiece, noPiece,   whiteBishop),
          Array(noPiece, blackPawn, noPiece),
          Array(noPiece, noPiece,   noPiece),
          Array(noPiece, noPiece,   noPiece),
          Array(noPiece, noPiece,   noPiece),
        )

        val game = new ChessGame(board)

        val expectedValidMoves = Set(
          Coordinate(1, 1),
        ).flatten

        MoveValidator.getValidMovesDownLeft(game, Coordinate(0, 2).get) shouldBe expectedValidMoves
      }

      "blocked ally white" in {
        val board: Board = Array(
          Array(noPiece, noPiece,   whiteBishop),
          Array(noPiece, whitePawn, noPiece),
          Array(noPiece, noPiece,   noPiece),
          Array(noPiece, noPiece,   noPiece),
          Array(noPiece, noPiece,   noPiece),
        )

        val game = new ChessGame(board)

        val expectedValidMoves = Set.empty

        MoveValidator.getValidMovesDownLeft(game, Coordinate(0, 2).get) shouldBe expectedValidMoves
      }
    }

    "validateDownLeft" should {
      "empty white" in {
        val board: Board = Array(
          Array(noPiece, noPiece, noPiece),
          Array(noPiece, noPiece, noPiece),
          Array(noPiece, noPiece, whiteBishop),
          Array(noPiece, noPiece, noPiece),
          Array(noPiece, noPiece, noPiece),
        )

        val game = new ChessGame(board)

        val expectedValidMoves = Set(
          Coordinate(1, 1),
          Coordinate(0, 0),
        ).flatten

        MoveValidator.getValidMovesUpLeft(game, Coordinate(2, 2).get) shouldBe expectedValidMoves
      }

      "empty white with limit" in {
        val board: Board = Array(
          Array(noPiece, noPiece, noPiece),
          Array(noPiece, noPiece, noPiece),
          Array(noPiece, noPiece, whiteBishop),
          Array(noPiece, noPiece, noPiece),
          Array(noPiece, noPiece, noPiece),
        )

        val game = new ChessGame(board)

        val expectedValidMoves = Set(
          Coordinate(1, 1),
        ).flatten

        MoveValidator.getValidMovesUpLeft(game, Coordinate(2, 2).get, 1) shouldBe expectedValidMoves
      }

      "blocked white" in {
        val board: Board = Array(
          Array(noPiece, noPiece,   noPiece),
          Array(noPiece, blackPawn, noPiece),
          Array(noPiece, noPiece,   whiteBishop),
          Array(noPiece, noPiece,   noPiece),
          Array(noPiece, noPiece,   noPiece),
        )

        val game = new ChessGame(board)

        val expectedValidMoves = Set(
          Coordinate(1, 1),
        ).flatten

        MoveValidator.getValidMovesUpLeft(game, Coordinate(2, 2).get) shouldBe expectedValidMoves
      }

      "blocked ally white" in {
        val board: Board = Array(
          Array(noPiece, noPiece,   noPiece),
          Array(noPiece, whitePawn, noPiece),
          Array(noPiece, noPiece,   whiteBishop),
          Array(noPiece, noPiece,   noPiece),
          Array(noPiece, noPiece,   noPiece),
        )

        val game = new ChessGame(board)

        val expectedValidMoves = Set.empty

        MoveValidator.getValidMovesUpLeft(game, Coordinate(2, 2).get) shouldBe expectedValidMoves
      }
    }

    "validateDownRight" should {
      "empty white" in {
        val board: Board = Array(
          Array(noPiece,     noPiece, noPiece),
          Array(noPiece,     noPiece, noPiece),
          Array(whiteBishop, noPiece, noPiece),
          Array(noPiece,     noPiece, noPiece),
          Array(noPiece,     noPiece, noPiece),
        )

        val game = new ChessGame(board)

        val expectedValidMoves = Set(
          Coordinate(1, 1),
          Coordinate(0, 2),
        ).flatten

        MoveValidator.getValidMovesUpRight(game, Coordinate(2, 0).get) shouldBe expectedValidMoves
      }

      "empty white with limit" in {
        val board: Board = Array(
          Array(noPiece,     noPiece, noPiece),
          Array(noPiece,     noPiece, noPiece),
          Array(whiteBishop, noPiece, noPiece),
          Array(noPiece,     noPiece, noPiece),
          Array(noPiece,     noPiece, noPiece),
        )

        val game = new ChessGame(board)

        val expectedValidMoves = Set(
          Coordinate(1, 1),
        ).flatten

        MoveValidator.getValidMovesUpRight(game, Coordinate(2, 0).get, 1) shouldBe expectedValidMoves
      }

      "blocked white" in {
        val board: Board = Array(
          Array(noPiece,     noPiece,   noPiece),
          Array(noPiece,     blackPawn, noPiece),
          Array(whiteBishop, noPiece,   noPiece),
          Array(noPiece,     noPiece,   noPiece),
          Array(noPiece,     noPiece,   noPiece),
        )

        val game = new ChessGame(board)

        val expectedValidMoves = Set(
          Coordinate(1, 1),
        ).flatten

        MoveValidator.getValidMovesUpRight(game, Coordinate(2, 0).get) shouldBe expectedValidMoves
      }

      "blocked ally white" in {
        val board: Board = Array(
          Array(noPiece,     noPiece,   noPiece),
          Array(noPiece,     whitePawn, noPiece),
          Array(whiteBishop, noPiece,   noPiece),
          Array(noPiece,     noPiece,   noPiece),
          Array(noPiece,     noPiece,   noPiece),
        )

        val game = new ChessGame(board)

        val expectedValidMoves = Set.empty

        MoveValidator.getValidMovesUpRight(game, Coordinate(2, 0).get) shouldBe expectedValidMoves
      }
    }

    "validateUp" should {
      "empty white" in {
        val board: Board = Array(
          Array(noPiece),
          Array(noPiece),
          Array(noPiece),
          Array(noPiece),
          Array(whiteRook),
        )
        val game = new ChessGame(board)

        val expectedValidMoves = Set(
          Coordinate(0, 0),
          Coordinate(1, 0),
          Coordinate(2, 0),
          Coordinate(3, 0),
        ).flatten

        MoveValidator.getValidMovesUp(game, Coordinate(4, 0).get) shouldBe expectedValidMoves
      }

      "empty white with limit" in {
        val board: Board = Array(
          Array(noPiece),
          Array(noPiece),
          Array(noPiece),
          Array(noPiece),
          Array(whiteRook),
        )
        val game = new ChessGame(board)

        val expectedValidMoves = Set(
          Coordinate(2, 0),
          Coordinate(3, 0),
        ).flatten

        MoveValidator.getValidMovesUp(game, Coordinate(4, 0).get, 2) shouldBe expectedValidMoves
      }

      "white enemy piece in the way" in {
        val board: Board = Array(
          Array(noPiece),
          Array(noPiece),
          Array(blackPawn),
          Array(noPiece),
          Array(whiteRook),
        )
        val game = new ChessGame(board)

        val expectedValidMoves = Set(
          Coordinate(2, 0),
          Coordinate(3, 0),
        ).flatten

        MoveValidator.getValidMovesUp(game, Coordinate(4, 0).get) shouldBe expectedValidMoves
      }

      "black ally piece in the way" in {
        val board: Board = Array(
          Array(noPiece),
          Array(noPiece),
          Array(whitePawn),
          Array(noPiece),
          Array(whiteRook),
        )
        val game = new ChessGame(board)

        val expectedValidMoves = Set(
          Coordinate(3, 0),
        ).flatten

        MoveValidator.getValidMovesUp(game, Coordinate(4, 0).get) shouldBe expectedValidMoves
      }
    }

    "validateDown" should {
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

        MoveValidator.getValidMovesDown(game, Coordinate(0, 0).get) shouldBe expectedValidMoves
      }

      "empty white with limit" in {
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
        ).flatten

        MoveValidator.getValidMovesDown(game, Coordinate(0, 0).get, 2) shouldBe expectedValidMoves
      }

      "white enemy piece in the way" in {
        val board: Board = Array(
          Array(whiteRook),
          Array(noPiece),
          Array(blackPawn),
          Array(noPiece),
          Array(noPiece),
        )
        val game = new ChessGame(board)

        val expectedValidMoves = Set(
          Coordinate(1, 0),
          Coordinate(2, 0),
        ).flatten

        MoveValidator.getValidMovesDown(game, Coordinate(0, 0).get) shouldBe expectedValidMoves
      }

      "black ally piece in the way" in {
        val board: Board = Array(
          Array(whiteRook),
          Array(noPiece),
          Array(whitePawn),
          Array(noPiece),
          Array(noPiece),
        )
        val game = new ChessGame(board)

        val expectedValidMoves = Set(
          Coordinate(1, 0),
        ).flatten

        MoveValidator.getValidMovesDown(game, Coordinate(0, 0).get) shouldBe expectedValidMoves
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

      "empty white with limit" in {
        val board: Board = Array(
          Array(noPiece, noPiece, noPiece, whiteRook),
        )
        val game = new ChessGame(board)

        val expectedValidMoves = Set(
          Coordinate(0, 1),
          Coordinate(0, 2),
        ).flatten

        MoveValidator.getValidMovesLeft(game, Coordinate(0, 3).get, 2) shouldBe expectedValidMoves
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

      "empty white with limit" in {
        val board: Board = Array(
          Array(whiteRook, noPiece, noPiece, noPiece),
        )
        val game = new ChessGame(board)

        val expectedValidMoves = Set(
          Coordinate(0, 1),
          Coordinate(0, 2),
        ).flatten

        MoveValidator.getValidMovesRight(game, Coordinate(0, 0).get, 2) shouldBe expectedValidMoves
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
