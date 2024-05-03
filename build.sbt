name := "ninety-nine-scala-problems"

scalaVersion := "2.13.14"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % Test

scalacOptions ++= Seq("-language:implicitConversions,higherKinds")
scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
