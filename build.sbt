val sversion = "2.10.3"

name := "dada"

version := "0.1.0"

licenses := Seq("LGPL v2.1+" -> url("http://www.gnu.org/licenses/lgpl-2.1.txt"))

scalaVersion := sversion

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies += "org.scala-lang" % "scala-reflect" % sversion

libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "0.4.2"

libraryDependencies += "net.databinder.dispatch" %% "dispatch-core" % "0.11.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"
// not yet, libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"
