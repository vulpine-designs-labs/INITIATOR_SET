ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.0.2"

lazy val root = (project in file("."))
	.dependsOn(utils)
	.settings(
		name := "VulpineDesigns",
		idePackagePrefix := Some("org.vulpine_designs.initiator_set")
	)
lazy val utils = (project in file("./Utils"))
	.settings(
		name := "Utils",
		idePackagePrefix := Some("org.vulpine_designs.initiator_set.utils")
	)
