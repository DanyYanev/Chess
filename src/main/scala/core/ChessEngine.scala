package core

import com.whitehatgaming.UserInput

class ChessEngine(val input: UserInput) {
  var game: ChessGame = new ChessGame()

  def run(): Unit = {
    var line = input.nextMove()
    while(line != null) {
      val move = for {
        from <- CoordinateDTO(line(1), line(0)).toCoordinate
        to <- CoordinateDTO(line(3), line(2)).toCoordinate
      } yield Move(from, to)

      move.foreach(validMove => game.nextTurn(validMove).foreach {
        validGame =>
          game = validGame
          game.printBoard()
      })
      line = input.nextMove()
    }
  }
}
