ThisBuild / version := "1.0"
ThisBuild / organization := "ch.epfl.lara"
ThisBuild / organizationName := "LARA"

ThisBuild / scalaVersion := "3.7.1"

lazy val root = (project in file("."))
  .settings(
    name := "orthologic",
    assembly / mainClass := Some ("Main"),
    assembly / assemblyJarName := "orthologic.jar",
    libraryDependencies += "com.zaxxer" % "SparseBitSet" % "1.2",
    libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.4.1",
  )

scalacOptions ++= Seq(
  "-unchecked", 
  "-deprecation",
  "-language:implicitConversions"
)

run / fork := true
run / javaOptions ++= Seq(
  "-Xss512M", // stack size
  "-Xmx4G", // (max) heap size
)
