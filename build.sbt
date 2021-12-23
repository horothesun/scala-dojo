lazy val root = (project in file(".")).settings(
  inThisBuild(
    List(
      organization := "com.horothesun",
      scalaVersion := "2.13.7"
    )
  ),
  name := "scala-dojo"
)

val munitVersion = "0.7.29"

libraryDependencies ++= Seq(
  "org.scalameta" %% "munit" % munitVersion % Test,
  "org.scalameta" %% "munit-scalacheck" % munitVersion % Test
)
