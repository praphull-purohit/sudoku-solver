import Dependencies._

ThisBuild / scalaVersion     := "2.13.2"
ThisBuild / organization     := "com.praphull"

lazy val root = (project in file("."))
  .settings(
    name := "sudoku-solver",
    version := "1.0.0",
    libraryDependencies += scalaTest % Test
  )

