import sbt.Keys.scalacOptions


lazy val scala2_11 = "2.11.12"
lazy val scala2_12 = "2.12.13"
lazy val scala2_13 = "2.13.5"
lazy val scala3 = "3.0.0-RC1"

lazy val commonSettings = Seq(
  organization := "se.umu.cs.rai.scopus",
  version := "0.5.0-SNAPSHOT",

  description := "Functional language to describe ethical problems",
  homepage := Some(url("https://bitbucket.org/mendezjulian/scopus")),
  startYear := Some(2020),
  licenses := Seq("Apache License Version 2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt")),
  organizationName := "Responsible Artificial Intelligence - Umea University",
  organizationHomepage := Some(url("https://www.umu.se/en/research/groups/responsible-artificial-intelligence")),
  developers := List(
    Developer("julianmendez", "Julian Mendez", "julian.mendez@gmail.com", new URL("https://julianmendez.github.io"))
  ),

  /**
   * Scala
   * [[https://www.scala-lang.org]]
   * [[https://github.com/scala/scala]]
   * [[https://repo1.maven.org/maven2/org/scala-lang/scalap/]]
   */
  crossScalaVersions := Seq(scala2_11, scala2_12, scala2_13, scala3),
  scalaVersion := scala3,

  /**
   * ScalaTest
   * [[http://www.scalatest.org]]
   * [[https://github.com/scalatest/scalatest]]
   * [[https://repo1.maven.org/maven2/org/scalatest/]]
   */
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.5" % "test",

  resolvers += Resolver.mavenLocal,
  publishTo := Some(Resolver.mavenLocal),
  publishMavenStyle := true,
  publishConfiguration := publishConfiguration.value.withOverwrite(true),
  publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true),
  scalacOptions ++= Seq("-deprecation", "-feature")
)


lazy val translator = project
  .withId(id = "translator")
  .in(file("translator"))
  .settings(
    commonSettings,
    mainClass in assembly := Some("scopus.translator.EntryPoint"),
    assemblyJarName in assembly := "translator-" + version.value + ".jar"
  )


lazy val library = project
  .withId(id = "library")
  .in(file("library"))
  .aggregate(translator)
  .dependsOn(translator)
  .settings(
    commonSettings,
    assemblyJarName in assembly := "library-" + version.value + ".jar"
  )


lazy val root = project
  .withId(id = "scopus")
  .in(file("."))
  .aggregate(translator, library)
  .dependsOn(translator, library)
  .settings(
    commonSettings,
    mainClass in assembly := Some("scopus.translator.EntryPoint"),
    assemblyJarName in assembly := "scopus-" + version.value + ".jar"
  )


