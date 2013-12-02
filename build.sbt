name := "DieStats"

version := "0.1.0"

scalaVersion := "2.10.3"

scalaHome := Some(file("/usr/local/scala"))

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-Xlint",
  "-target:jvm-1.6",
  "-encoding", "UTF-8",
  "-feature",
  "-optimise",
  "-Yinline-warnings"
)

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.10" % "2.0" % "test"
)

initialCommands in console += "import net.paploo.diestats._;"
