name := "DieStats"

version := "0.3.0"

scalaVersion := "2.13.6"

scalacOptions in (Compile, compile) ++= Seq(
  "-unchecked",
  "-deprecation",
  "-Xlint",
  "-target:jvm-1.8",
  "-encoding", "UTF-8",
  "-feature",
  "-Wunused:imports,locals",
  "-Xlint:infer-any"
)

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.3",
  "org.scalatest" %% "scalatest" % "3.2.9" % "test"
)

initialCommands in console += "import net.paploo.diestats._;"
initialCommands in console += "import net.paploo.diestats.expression._;"
initialCommands in console += "import net.paploo.diestats.statistics._;"

scalacOptions in (Compile,doc) := Seq("-groups", "-implicits")

autoAPIMappings := true
