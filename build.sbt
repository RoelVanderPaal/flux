ThisBuild / organization := "flux"
ThisBuild / version      := "0.0.1-SNAPSHOT"
ThisBuild / scalaVersion := "3.2.2"

lazy val core = project
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalacOptions ++= Seq("-deprecation", "-feature", "-language:implicitConversions"),
    libraryDependencies ++= Seq(
      "org.scala-js"  %%% "scalajs-dom" % "2.4.0",
      "org.scalatest" %%% "scalatest"   % "3.2.15" % Test
    ),
    jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()
  )

lazy val example = project
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalaJSUseMainModuleInitializer := true,
    scalacOptions ++= Seq("-deprecation", "-feature", "-language:implicitConversions")
  )
  .dependsOn(core)

lazy val todomvc = (project in file("examples/todomvc"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalaJSUseMainModuleInitializer := true,
    scalacOptions ++= Seq("-deprecation", "-feature", "-language:implicitConversions")
  )
  .dependsOn(core)
