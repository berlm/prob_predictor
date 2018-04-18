name := "prob_predictor"

version := "0.1"

//scalaVersion := "2.12.0"
scalaVersion := "2.11.7"


libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-swing" % "1.0.1",
  "org.jliszka" %% "probability-monad" % "1.0.1"
)


libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5"
libraryDependencies += "junit" % "junit" % "4.10" % Test
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % Test

