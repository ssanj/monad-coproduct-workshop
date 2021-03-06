name := "monad-coproduct-workshop"

version := "1.0"

scalaVersion := "2.11.8"

val catsVersion = "0.7.0"

val monixVersion = "2.0-RC13"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats" % catsVersion,
  "io.monix" %% "monix" % monixVersion,
  "io.monix" %% "monix-cats" % monixVersion
)
