name := "scala-code"

version := "0.0.1-SNAPSHOT"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-language:implicitConversions")

scalaVersion := "2.13.2"

organization := "com.github.arzt"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.1.2" % Test,
  "org.specs2" %% "specs2-core" % "4.6.0" % "test",
  "org.jblas" % "jblas" % "1.2.4",
  "org.scala-lang.modules" %% "scala-collection-compat" % "2.1.2",
  "com.github" %% "scala-tensor" % "0.0.1-SNAPSHOT"
)

crossScalaVersions := List("2.13.2", "2.12.11", "2.11.12")
