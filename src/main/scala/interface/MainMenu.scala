package interface

import interface.MainMenu.menuOptions

case class MainMenu() extends MenuItem {
  override val desc = "Main menu"
  override def body(): MenuItem = {
    printMenuItems(menuOptions)
    handleInput(menuOptions)
  }
}

object MainMenu {
  val menuOptions = List(
    SelectGame(),
    Exit()
  )
}