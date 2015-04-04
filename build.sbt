name := """grammar-web"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

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

