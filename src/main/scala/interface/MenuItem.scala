package interface

import scala.util.control.Breaks.{break, breakable}

trait MenuItem {
  val desc: String = ""
  def body(): MenuItem = this

  //Convenient for menu behaviour
  final def run(): Unit = {
    breakable {
      while(true)
        body() match {
          case Exit() => break
          case item => item.run()
        }
    }
  }

  protected def printMenuItems(options: List[MenuItem]): Unit = {
    for((option, i) <- options zip (LazyList from 1)) {
      println(s"$i. ${option.desc}")
    }
  }

  protected def handleInput(options: List[MenuItem]): MenuItem = {
    var choice = -1
    do {
      print("Enter choice: ")
      choice = scala.io.StdIn.readInt()
    } while (choice > options.length || choice < 1)

    options(choiceToIndex(choice))
  }

  protected def choiceToIndex(choice: Int): Int = choice - 1
}

case class Exit() extends MenuItem {
  override val desc: String = "Exit"
  override val body: MenuItem = this
}