ThisBuild / scalaVersion := "3.10.0"
ThisBuild / organization := "cz.judas.jan"

lazy val hello = project
  .in(file("."))
  .settings(
    name := "advent-of-scala",
  )

libraryDependencies ++= Seq(
  "com.google.guava" % "guava" % "33.8.0-jre",

  "com.github.sbt.junit" % "jupiter-interface" % JupiterKeys.jupiterVersion.value % Test,
  "org.scalatest" %% "scalatest" % "3.2.20" % Test
)
 
enablePlugins(PackPlugin)

//TODO
scalacOptions ++= Seq(
  "-Xcheck-macros"
)
