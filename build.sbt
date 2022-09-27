
ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.3"

lazy val root = (project in file("."))
  .settings(
    name := "lattices-algorithms"
  )

scalacOptions := Seq("-unchecked", "-deprecation")

run / fork := true
run / javaOptions ++= Seq(
  "-Xss512M", // stack size
  "-Xmx4G", // (max) heap size
)

enablePlugins(JmhPlugin)