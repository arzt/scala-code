name := "scala-code"

version := "0.0.1-SNAPSHOT"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-language:implicitConversions")

scalaVersion := "3.3.3"

organization := "com.github.arzt"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.18" % Test,
  "org.jblas" % "jblas" % "1.2.5",
  "org.scala-lang.modules" %% "scala-collection-compat" % "2.12.0"
)

//crossScalaVersions := List("3.2.1", "2.13.6", "2.12.15", "2.11.12")
