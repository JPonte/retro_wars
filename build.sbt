ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "retro_wars",
    idePackagePrefix := Some("org.jponte")
  )

val circeVersion = "0.14.1"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

libraryDependencies ++= Seq(
  "dev.optics" %% "monocle-core"  % "3.1.0",
  "dev.optics" %% "monocle-macro" % "3.1.0",
)