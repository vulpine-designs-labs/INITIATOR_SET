
name := "InitiatorSet"

version := "0.0.1"

scalaVersion := "3.0.2"

val scalactic = "org.scalactic" %% "scalactic" % "3.2.10"
val scalaTest = "org.scalatest" %% "scalatest" % "3.2.10"

lazy val root = (project in file("."))
	.settings(
		name := "InitiatorSet",
		idePackagePrefix := Some("org.vulpine_designs.initiator_set"),
		libraryDependencies += scalactic,
		libraryDependencies += scalaTest % "test"
	)
