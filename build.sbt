name := "Chess"

version := "0.1"

scalaVersion := "2.13.4"

val enumeratumVersion = "1.6.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"
libraryDependencies += "org.scalatest" %% "scalatest-wordspec" % "3.2.2" % "test"

libraryDependencies += "com.beachape" %% "enumeratum" % enumeratumVersion