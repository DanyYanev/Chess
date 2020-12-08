package core

import com.whitehatgaming.UserInputFile

class ChessEngine(val input: UserInputFile) {
  var game: ChessGame = new ChessGame

  def run() = {
    game.printBoard
  }
}

object ChessEngine {

}