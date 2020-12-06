package interface

import com.whitehatgaming.UserInputFile

object Main extends App {
  val user = new UserInputFile("data/checkmate.txt")
  val menu = MainMenu()
  menu.run()
}

