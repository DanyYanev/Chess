package interface

import interface.MainMenu.menuOptions

case class MainMenu() extends MenuItem {
  override val desc = "Main menu"
  override def body(): MenuItem = {
    for((option, i) <- menuOptions zip (LazyList from 1)) {
      println(s"$i. $option")
    }

    print("Enter choice: ")
    val choice = scala.io.StdIn.readInt()
    menuOptions(choiceToIndex(choice))
  }
}

object MainMenu{
  val menuOptions = List(
    SelectGame(),
    Exit()
  )
}