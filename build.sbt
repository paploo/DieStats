name := "DieStats"

version := "0.1.0"

scalaVersion := "2.11.0"

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
  "org.scalatest" % "scalatest_2.11" % "2.1.4" % "test"
)

initialCommands in console += "import net.paploo.diestats._;"

scalacOptions in (Compile,doc) := Seq("-groups", "-implicits")

autoAPIMappings := true
