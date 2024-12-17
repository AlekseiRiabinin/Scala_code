ThisBuild / scalaVersion := "3.3.4"

lazy val root = (project in file("."))
  .settings(
    name := "image-convolution",
    version := "0.1.0",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-swing" % "3.0.0",
      "org.scalatest" %% "scalatest" % "3.2.16" % Test
    ),
    Compile / run / mainClass := Some("ImageConvolution")
  )
