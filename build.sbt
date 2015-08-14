name := """grammar-web"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala).aggregate(aceJsProject, js).dependsOn(shared)

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

lazy val js = (project in file("js")).enablePlugins(ScalaJSPlugin).settings(
  libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.8.0",
  libraryDependencies += "be.doeraene" %%% "scalajs-jquery" % "0.8.0",
  libraryDependencies += "com.github.japgolly.scalajs-react" %%% "core" % "0.9.0",
  jsDependencies +=
  "org.webjars" % "react" % "0.12.2" / "react-with-addons.js" commonJSName "React",
  unmanagedSourceDirectories in Compile += baseDirectory.value / "../shared/src"
).dependsOn(aceJsProject)

scalaVersion in js := "2.11.1"

lazy val shared = (project in file("shared"))
scalaVersion in shared := "2.11.1"

lazy val copyjs = TaskKey[Unit]("copyjs", "Copy javascript files to target directory")

copyjs := {
  val outDir = baseDirectory.value / "public/js"
  val inDir = baseDirectory.value / "js/target/scala-2.11"
  val files = Seq("js-fastopt.js", "js-fastopt.js.map", "js-jsdeps.js") map { p => (inDir / p, outDir / p) }
  IO.copy(files, true)
}

watchSources := watchSources.value.filter ( source => Seq("js-fastopt.js", "js-fastopt.js.map", "js-jsdeps.js").indexOf(source.getName) == -1 )

addCommandAlias("runServer", ";fastOptJS;copyjs;run")
addCommandAlias("fastOptCopy", ";fastOptJS;copyjs")

lazy val aceJsProject = RootProject(uri("https://github.com/MikaelMayer/scalajs-ace.git"))
