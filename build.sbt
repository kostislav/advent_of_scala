ThisBuild / scalaVersion := "3.6.1"
ThisBuild / organization := "cz.judas.jan"

lazy val hello = project
  .in(file("."))
  .settings(
    name := "advent-of-scala",
  )
 
enablePlugins(PackPlugin)