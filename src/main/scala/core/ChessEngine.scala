package core

import com.whitehatgaming.UserInput

class ChessEngine(val input: UserInput) {
  var game: ChessGame = new ChessGame()

  def run() = {
    var line = input.nextMove()
    while(line != null) {
      val move = for {
        from <- Coordinate(8 - line(1) - 1, line(0))
        to <- Coordinate(8 - line(3) - 1, line(2))
      } yield Move(from, to)

      move.foreach(validMove => game.nextTurn(validMove).foreach {
        validGame =>
          game = validGame
          game.printBoard
      })
      line = input.nextMove()
    }
  }
}

object ChessEngine {

}