import scala.Seq

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.0.2"

val scalactic = "org.scalactic" %% "scalactic" % "3.2.10"
val scalaTest = "org.scalatest" %% "scalatest" % "3.2.10"

lazy val root = (project in file("."))
	.dependsOn(utils)
	.settings(
		name := "VulpineDesigns",
		idePackagePrefix := Some("org.vulpine_designs.initiator_set"),
		libraryDependencies += scalactic,
		libraryDependencies += scalaTest % "test"
	)
lazy val utils = (project in file("./Utils"))
	.settings(
		name := "Utils",
		idePackagePrefix := Some("org.vulpine_designs.initiator_set.utils"),
		libraryDependencies += scalactic,
		libraryDependencies += scalaTest % "test"
	)
