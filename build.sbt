import sbt.Keys.scalacOptions


lazy val scala2 = "2.13.3"
lazy val scala3 = "0.27.0-RC1"

lazy val commonSettings = Seq(
  organization := "se.umu.cs.rai.scopus",
  version := "0.1.0",

  /**
   * Scala
   * [[https://www.scala-lang.org]]
   * [[https://github.com/scala/scala]]
   * [[https://repo1.maven.org/maven2/org/scala-lang/scalap/]]
   */
  crossScalaVersions := Seq(scala2, scala3),
  scalaVersion := scala2,

  /**
   * ScalaTest
   * [[http://www.scalatest.org]]
   * [[https://github.com/scalatest/scalatest]]
   * [[https://repo1.maven.org/maven2/org/scalatest/scalatest_2.13/]]
   */
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test",

  resolvers += Resolver.mavenLocal,
  publishTo := Some(Resolver.mavenLocal),
  publishMavenStyle := true,
  scalacOptions ++= Seq("-deprecation", "-feature")
)


lazy val translator = project
  .withId(id = "translator")
  .in(file("translator"))
  .settings(
    commonSettings,
    mainClass in assembly := Some("se.umu.cs.rai.scopus.translator.Main"),
    assemblyJarName in assembly := "translator-" + version.value + ".jar"
  )


lazy val root = project
  .withId(id = "scopus")
  .in(file("."))
  .aggregate(translator)
  .dependsOn(translator)
  .settings(
    commonSettings,
    mainClass in assembly := Some("se.umu.cs.rai.scopus.translator.Main"),
    assemblyJarName in assembly := "scopus-" + version.value + ".jar"
  )

