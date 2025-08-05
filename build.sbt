
ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.1"

lazy val root = (project in file("."))
  .settings(
    name := "lattices-algorithms",
    assembly / mainClass := Some ("Main"),
    assembly / assemblyJarName := "lattices.jar",
    libraryDependencies += "com.zaxxer" % "SparseBitSet" % "1.2",
    libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.4.1",
  )

scalacOptions := Seq("-unchecked", "-deprecation")
scalacOptions ++= Seq(
  "-language:implicitConversions",
  "-language:experimental.modularity"
)

run / fork := true
run / javaOptions ++= Seq(
  "-Xss512M", // stack size
  "-Xmx4G", // (max) heap size
)
