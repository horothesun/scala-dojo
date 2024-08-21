val catsVersion = "2.12.0"

val kittensVersion = "3.4.0"

val catsParseVersion = "1.0.0"

val catsEffectVersion = "3.5.4"

val betterMonadicForVersion = "0.3.1"

val fs2Version = "3.10.2"

val munitVersion = "1.0.1"

val munitScalacheckVersion = "1.0.0"

val munitCatsEffectVersion = "2.0.0"

val scalacheckVersion = "1.18.0"

val scalacheckEffectMunitVersion = "1.0.4"

val disciplineMunitVersion = "2.0.0"

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
  "org.scalameta" %% "munit" % munitVersion % Test,
  "org.typelevel" %% "munit-cats-effect" % munitCatsEffectVersion % Test,
  "org.scalameta" %% "munit-scalacheck" % munitScalacheckVersion % Test,
  "org.scalacheck" %% "scalacheck" % scalacheckVersion % Test,
  "org.typelevel" %% "scalacheck-effect-munit" % scalacheckEffectMunitVersion % Test,
  "org.typelevel" %% "cats-effect-testkit" % catsEffectVersion % Test,
  "org.typelevel" %% "cats-laws" % catsVersion % Test,
  "org.typelevel" %% "discipline-munit" % disciplineMunitVersion % Test
)
