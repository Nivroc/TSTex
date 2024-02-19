ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "TSTex"
  )

libraryDependencies += "org.typelevel" %% "cats-core" % "2.9.0"

scalacOptions ++= Seq(
  "-Wconf:msg=kind-projector:silent",
  "-unchecked",
  "-language:implicitConversions",
  "-language:higherKinds",
  "-Ymacro-annotations",
  "-Xlog-implicits"
)