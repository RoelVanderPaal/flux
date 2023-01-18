ThisBuild / organization := "flux"
ThisBuild / version      := "0.0.1-SNAPSHOT"
ThisBuild / scalaVersion := "3.2.2"

lazy val core = project
  .enablePlugins(ScalaJSPlugin)
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-js"  %%% "scalajs-dom" % "2.3.0",
      "org.scalatest" %%% "scalatest"   % "3.2.15" % Test
    )
  )

lazy val example = project
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalaJSUseMainModuleInitializer := true
  )
  .dependsOn(core)
