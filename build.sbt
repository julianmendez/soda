import sbt.Keys.scalacOptions

lazy val scala2_11 = "2.11.12"

lazy val scala2_12 = "2.12.14"

lazy val scala2_13 = "2.13.8"

lazy val scala3_0 = "3.0.2"

lazy val scala3_1 = "3.1.1"

lazy val commonSettings =
  Seq(
    organization := "se.umu.cs.rai.soda",
    version := "0.16.0-SNAPSHOT",
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
    crossScalaVersions := Seq(scala2_11, scala2_12, scala2_13, scala3_0, scala3_1),
    scalaVersion := scala3_1,
    /**
     * ScalaTest
     * [[http://www.scalatest.org]]
     * [[https://github.com/scalatest/scalatest]]
     * [[https://repo1.maven.org/maven2/org/scalatest/]]
     */
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.11" % "test",
    resolvers += Resolver.mavenLocal,
    publishTo := Some(Resolver.mavenLocal),
    publishMavenStyle := true,
    publishConfiguration := publishConfiguration.value.withOverwrite(true),
    publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true),
    scalacOptions ++= Seq("-deprecation", "-feature")
  )

lazy val documentation =
  project
    .withId("documentation")
    .in(file("documentation"))
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

lazy val library =
  project
    .withId("library")
    .in(file("library"))
    .aggregate(translator)
    .dependsOn(translator)
    .settings(
      commonSettings,
      assembly / assemblyJarName := "library-" + version.value + ".jar"
    )

lazy val examples =
  project
    .withId("examples")
    .in(file("examples"))
    .aggregate(translator, library)
    .dependsOn(translator, library)
    .settings(
      commonSettings,
      assembly / assemblyJarName := "examples-" + version.value + ".jar"
    )

lazy val root =
  project
    .withId("soda")
    .in(file("."))
    .aggregate(documentation, translator, library, examples)
    .dependsOn(documentation, translator, library, examples)
    .settings(
      commonSettings,
      assembly / mainClass := Some("soda.translator.extension.main.EntryPoint"),
      assembly / assemblyJarName := "soda-" + version.value + ".jar"
    )

