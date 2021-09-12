import sbt.Keys.scalacOptions


lazy val scala2_11 = "2.11.12"
lazy val scala2_12 = "2.12.14"
lazy val scala2_13 = "2.13.6"
lazy val scala3 = "3.0.2"

lazy val commonSettings = Seq(
  organization := "se.umu.cs.rai.soda",
  version := "0.11.0",

  description := "Functional language to describe ethical problems",
  homepage := Some(url("https://bitbucket.org/mendezjulian/soda")),
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
   * [[https://repo1.maven.org/maven2/org/scala-lang/scala3-compiler_3/]]
   */
  crossScalaVersions := Seq(scala2_11, scala2_12, scala2_13, scala3),
  scalaVersion := scala3,

  /**
   * ScalaTest
   * [[http://www.scalatest.org]]
   * [[https://github.com/scalatest/scalatest]]
   * [[https://repo1.maven.org/maven2/org/scalatest/]]
   */
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test",

  resolvers += Resolver.mavenLocal,
  publishTo := Some(Resolver.mavenLocal),
  publishMavenStyle := true,
  publishConfiguration := publishConfiguration.value.withOverwrite(true),
  publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true),
  scalacOptions ++= Seq("-deprecation", "-feature")
)


lazy val documentation = project
  .withId(id = "documentation")
  .in(file("documentation"))
  .settings(commonSettings)


lazy val translator = project
  .withId(id = "translator")
  .in(file("translator"))
  .settings(
    commonSettings,
    assembly / mainClass := Some("soda.translator.io.EntryPoint"),
    assembly / assemblyJarName := "translator-" + version.value + ".jar"
  )


lazy val coqport = project
  .withId(id = "coqport")
  .in(file("coqport"))
  .aggregate(translator)
  .dependsOn(translator)
  .settings(
    commonSettings,
    assembly / mainClass := Some("soda.coqport.io.EntryPoint"),
    assembly / assemblyJarName := "coqport-" + version.value + ".jar"
  )


lazy val library = project
  .withId(id = "library")
  .in(file("library"))
  .aggregate(translator)
  .dependsOn(translator)
  .settings(
    commonSettings,
    assembly / assemblyJarName := "library-" + version.value + ".jar"
  )


lazy val examples = project
  .withId(id = "examples")
  .in(file("examples"))
  .aggregate(translator, library)
  .dependsOn(translator, library)
  .settings(
    commonSettings,
    assembly / assemblyJarName := "examples-" + version.value + ".jar"
  )


lazy val root = project
  .withId(id = "soda")
  .in(file("."))
  .aggregate(documentation, translator, coqport, library, examples)
  .dependsOn(documentation, translator, coqport, library, examples)
  .settings(
    commonSettings,
    assembly / mainClass := Some("soda.translator.io.EntryPoint"),
    assembly / assemblyJarName := "soda-" + version.value + ".jar"
  )


