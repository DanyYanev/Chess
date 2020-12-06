package interface

import java.io.File

case class SelectGame() extends MenuItem {
  override val desc = "Main menu"
  lazy val games = SelectGame.getListOfFiles(SelectGame.gamesDir)

  override def body(): MenuItem = {
    for((option, i) <- games zip (LazyList from 1)) {
      println(s"$i. ${option.getName}")
    }

    print("Enter choice: ")
    val choice = scala.io.StdIn.readInt()
    println(s"Playing ${games(choiceToIndex(choice))}")
    this
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
}