package interface

import com.whitehatgaming.UserInputFile
import core.ChessEngine
import interface.SelectGame.toMenuItem

import java.io.File

case class SelectGame() extends MenuItem {
  override val desc = "Select game"
  lazy val games: List[MenuItem] = SelectGame.getListOfFiles(SelectGame.gamesDir).map(toMenuItem) :+ Exit()

  override def body(): MenuItem = {
    printMenuItems(games)
    handleInput(games)
  }
}

object SelectGame {
  val gamesDir = "data"

  private def getListOfFiles(dir: String):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }

  private def toMenuItem(file: File): MenuItem = {
    new MenuItem {
      override val desc: String = file.getName

      override def body(): MenuItem = {
        println(s"Playing ${file.getName}")
        val input = new UserInputFile(file.getAbsolutePath)
        val engine = new ChessEngine(input).run()
        Exit()
      }
    }
  }
}