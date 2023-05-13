lazy val root = (project in file(".")).settings(
  inThisBuild(
    List(
      organization := "com.horothesun",
      scalaVersion := "2.13.10"
    )
  ),
  name := "scala-dojo"
)

val catsVersion = "2.9.0"
val catsEffectVersion = "3.4.11"
val betterMonadicForVersion = "0.3.1"
val fs2Version = "3.7.0"
val munitVersion = "0.7.29"
val disciplineMunitVersion = "1.0.9"
val munitCatsEffectVersion = "1.0.7"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-effect" % catsEffectVersion,
  compilerPlugin("com.olegpy" %% "better-monadic-for" % betterMonadicForVersion),
  "co.fs2" %% "fs2-core" % fs2Version,
  "org.typelevel" %% "cats-laws" % catsVersion % Test,
  "org.scalameta" %% "munit" % munitVersion % Test,
  "org.scalameta" %% "munit-scalacheck" % munitVersion % Test,
  "org.typelevel" %% "discipline-munit" % disciplineMunitVersion % Test,
  "org.typelevel" %% "munit-cats-effect-3" % munitCatsEffectVersion % Test
)
