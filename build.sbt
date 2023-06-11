ThisBuild / organization := "flux"
ThisBuild / version      := "0.0.1-SNAPSHOT"
ThisBuild / scalaVersion := "3.3.0"

val scalajsDomVersion = "2.6.0"
val scalatestVersion  = "3.2.16"
lazy val streams      = project
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalacOptions ++= Seq("-deprecation", "-feature", "-language:implicitConversions"),
    libraryDependencies ++= Seq(
      "org.scala-js"  %%% "scalajs-dom" % scalajsDomVersion,
      "org.scalatest" %%% "scalatest"   % scalatestVersion % Test
    ),
    jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()
  )

lazy val core = project
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalacOptions ++= Seq("-deprecation", "-feature", "-language:implicitConversions"),
    libraryDependencies ++= Seq(
      "org.scala-js"  %%% "scalajs-dom" % scalajsDomVersion,
      "org.scalatest" %%% "scalatest"   % scalatestVersion % Test
    ),
    jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()
  )
  .dependsOn(streams)

lazy val core_new = project
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalacOptions ++= Seq("-deprecation", "-feature", "-language:implicitConversions"),
    libraryDependencies ++= Seq(
      "org.scala-js"  %%% "scalajs-dom" % scalajsDomVersion,
      "org.scalatest" %%% "scalatest"   % scalatestVersion % Test
    ),
    jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()
  )
  .dependsOn(streams)

//lazy val example = project
//  .enablePlugins(ScalaJSPlugin)
//  .settings(
//    scalaJSUseMainModuleInitializer := true,
//    scalacOptions ++= Seq("-deprecation", "-feature", "-language:implicitConversions")
//  )
//  .dependsOn(core)
import org.scalajs.linker.interface.ModuleSplitStyle
lazy val todomvc = (project in file("examples/todomvc"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalaJSUseMainModuleInitializer := true,
    scalacOptions ++= Seq("-deprecation", "-feature", "-language:implicitConversions"),
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule).withModuleSplitStyle(ModuleSplitStyle.SmallModulesFor(List("todomvc"))) }
  )
  .dependsOn(core_new)
