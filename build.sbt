
name := "initiator_set"

version := "0.0.1"

scalaVersion := "3.0.2"

val scalactic = "org.scalactic" %% "scalactic" % "3.2.11"
val scalaTest = "org.scalatest" %% "scalatest" % "3.2.11"

lazy val root = (project in file("."))
	.settings(
		name := "initiator_set",
		idePackagePrefix := Some("org.vulpinedesigns.initiator_set"),
		libraryDependencies += scalactic,
		libraryDependencies += scalaTest % "test"
	)
