name := "DieStats"

version := "0.2.0"

scalaVersion := "2.12.1"

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-Xlint",
  "-target:jvm-1.8",
  "-encoding", "UTF-8",
  "-feature",
  "-Ywarn-unused",
  "-Ywarn-unused-import",
  "-Ywarn-infer-any"
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

initialCommands in console += "import net.paploo.diestats._;"

scalacOptions in (Compile,doc) := Seq("-groups", "-implicits")

autoAPIMappings := true
