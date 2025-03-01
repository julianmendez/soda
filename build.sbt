import sbt.Keys.scalacOptions

lazy val scala2_11 = "2.11.12"

lazy val scala2_12 = "2.12.20"

lazy val scala2_13 = "2.13.16"

lazy val scala3_3 = "3.3.5"

lazy val scala3_6 = "3.6.3"

lazy val commonSettings =
  Seq(
    organization := "se.umu.cs.rai.soda",
    version := "0.21.0-SNAPSHOT",
    description := "Object-oriented functional language to describe, analyze, and model human-centered problems",
    homepage := Some(url("https://julianmendez.github.io/soda/")),
    startYear := Some(2020),
    licenses := Seq("Apache License Version 2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt")),
    organizationName := "Umea University",
    organizationHomepage := Some(url("https://www.umu.se/en/department-of-computing-science/")),
    developers := List(
      Developer("julianmendez", "Julian Alfredo Mendez", "julian.mendez@gmail.com", url("https://julianmendez.github.io"))
    ),
    /**
     * Scala
     * [[https://www.scala-lang.org]]
     * [[https://github.com/scala/scala]]
     * [[https://repo1.maven.org/maven2/org/scala-lang/scalap/]]
     * [[https://repo1.maven.org/maven2/org/scala-lang/scala3-compiler_3/]]
     */
    crossScalaVersions := Seq(scala2_11, scala2_12, scala2_13, scala3_3, scala3_6),
    scalaVersion := scala3_3,
    /**
     * ScalaTest
     * [[https://www.scalatest.org]]
     * [[https://github.com/scalatest/scalatest]]
     * [[https://repo1.maven.org/maven2/org/scalatest/]]
     */
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test",
    resolvers += Resolver.mavenLocal,
    publishTo := Some(Resolver.mavenLocal),
    publishMavenStyle := true,
    publishConfiguration := publishConfiguration.value.withOverwrite(true),
    publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true),
    scalacOptions ++= Seq("-deprecation", "-feature")
  )

lazy val docs =
  project
    .withId("docs")
    .in(file("docs"))
    .settings(commonSettings)

lazy val translator =
  project
    .withId("translator")
    .in(file("translator"))
    .settings(
      commonSettings,
      assembly / mainClass := Some("soda.translator.extension.main.EntryPoint"),
      assembly / assemblyJarName := "translator-" + version.value + ".jar"
    )

lazy val tiles =
  project
    .withId("tiles")
    .in(file("tiles"))
    .settings(
      commonSettings,
      assembly / assemblyJarName := "tiles-" + version.value + ".jar"
    )

lazy val examples =
  project
    .withId("examples")
    .in(file("examples"))
    .aggregate(translator)
    .dependsOn(translator)
    .settings(
      commonSettings,
      assembly / assemblyJarName := "examples-" + version.value + ".jar"
    )

lazy val root =
  project
    .withId("soda")
    .in(file("."))
    .aggregate(docs, translator, tiles, examples)
    .dependsOn(docs, translator, tiles, examples)
    .settings(
      commonSettings,
      assembly / mainClass := Some("soda.translator.extension.main.EntryPoint"),
      assembly / assemblyJarName := "soda-" + version.value + ".jar"
    )

