lazy val root = (project in file(".")).settings(
  inThisBuild(
    List(
      organization := "com.horothesun",
      scalaVersion := "2.13.7"
    )
  ),
  name := "scala-dojo"
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.9" % Test,
  "org.scalacheck" %% "scalacheck" % "1.15.4" % "test"
)
