name := "scala-commonmark"

version := "0.1"

scalaVersion := "2.11.2"

scalacOptions ++= Seq("-feature")

libraryDependencies += "com.scalatags" %% "scalatags" % "0.4.2"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.0"
