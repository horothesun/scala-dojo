val catsVersion = "2.10.0"

val kittensVersion = "3.3.0"

val catsParseVersion = "1.0.0"

val catsEffectVersion = "3.5.4"

val betterMonadicForVersion = "0.3.1"

val fs2Version = "3.10.2"

val munitVersion = "1.0.0"

val disciplineMunitVersion = "2.0.0"

val munitCatsEffectVersion = "2.0.0"

lazy val root = (project in file(".")).settings(
  inThisBuild(
    List(
      organization := "com.horothesun",
      scalaVersion := "2.13.14"
    )
  ),
  name := "scala-dojo",
  scalacOptions := Seq("-unchecked", "-deprecation")
)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "kittens" % kittensVersion,
  "org.typelevel" %% "cats-parse" % catsParseVersion,
  "org.typelevel" %% "cats-effect" % catsEffectVersion,
  compilerPlugin("com.olegpy" %% "better-monadic-for" % betterMonadicForVersion),
  "co.fs2" %% "fs2-core" % fs2Version,
  "org.typelevel" %% "cats-laws" % catsVersion % Test,
  "org.scalameta" %% "munit" % munitVersion % Test,
  "org.scalameta" %% "munit-scalacheck" % munitVersion % Test,
  "org.typelevel" %% "discipline-munit" % disciplineMunitVersion % Test,
  "org.typelevel" %% "munit-cats-effect" % munitCatsEffectVersion % Test
)
