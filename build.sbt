name := """grammar-web"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala).aggregate(js)

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  ws
)

unmanagedClasspath in Runtime += baseDirectory.value / "public/resources/bin"

fork in run := true

javaOptions += "-Xmx30G"

javaOptions += "-Xss20m"

javaOptions += "-Xms5G"

lazy val js = (project in file("js")).enablePlugins(ScalaJSPlugin)

scalaVersion in js := "2.11.1"

lazy val copyjs = TaskKey[Unit]("copyjs", "Copy javascript files to target directory")

copyjs := {
  val outDir = baseDirectory.value / "public/js"
  val inDir = baseDirectory.value / "js/target/scala-2.11"
  val files = Seq("js-fastopt.js", "js-fastopt.js.map", "js-jsdeps.js") map { p => (inDir / p, outDir / p) }
  IO.copy(files, true)
}

addCommandAlias("runServer", ";fastOptJS;copyjs;run")