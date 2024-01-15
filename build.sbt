ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

//enablePlugins(ScalaNativePlugin)

//import scala.scalanative.build._

lazy val root = (project in file("."))
  .settings(
    name             := "pivot",
    idePackagePrefix := Some("pivot"),
    libraryDependencies ++= Seq(
      "com.lihaoyi"   %% "fastparse" % "3.0.2",
      "org.scalameta" %% "munit"     % "1.0.0-M10" % Test
    )
  )
