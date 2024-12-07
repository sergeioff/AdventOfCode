ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.5.2"

lazy val root = (project in file("."))
  .settings(
    name := "AdventOfCode",
  )
  .aggregate(
    `y2024`
  )

lazy val `y2024` = (project in file("2024"))
  .settings(
    name := "2024",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.19" % Test
    )
  )