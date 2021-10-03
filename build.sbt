lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.horothesun",
      scalaVersion := "2.13.6"
    )),
    name := "scala-dojo"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test
