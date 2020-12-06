package interface

import scala.util.control.Breaks.{break, breakable}

trait MenuItem {
  val desc: String = " "
  def body(): MenuItem = this

  final def run(): Unit = {
    breakable {
      while(true)
        body() match {
          case Exit() => break
          case item => item.run()
        }
    }
  }

  def choiceToIndex(choice: Int): Int = choice - 1
}

case class Exit() extends MenuItem {
  override val desc: String = "Exit"
  override val body: MenuItem = this
}