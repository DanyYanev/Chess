package interface

import com.whitehatgaming.UserInputFile
import java.io.File

object Main extends App {
  val obj = new File("data/checkmate.txt")
  println(obj.exists())
  println(obj.getAbsolutePath())
  val user = new UserInputFile("data/checkmate.txt")
  println(user.nextMove())
  println("Works")
}

