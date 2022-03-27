import scala.Seq

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.0.2"

val scalactic = "org.scalactic" %% "scalactic" % "3.2.10"
val scalaTest = "org.scalatest" %% "scalatest" % "3.2.10"

lazy val root = (project in file("."))
	.dependsOn(utils)
	.dependsOn(kozakCalc)
	.dependsOn(mapAIC)
	.dependsOn(lsd)
	.settings(
		name := "InitiatorSet",
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
lazy val kozakCalc = (project in file("./KozakCalc"))
	.dependsOn(utils)
	.settings(
		name := "KozakCalc",
		idePackagePrefix := Some("org.vulpine_designs.initiator_set.kozak_calc"),
		libraryDependencies += scalactic,
		libraryDependencies += scalaTest % "test"
	)
lazy val mapAIC = (project in file("./MapAIC"))
	.dependsOn(utils)
	.settings(
		name := "MapAIC",
		idePackagePrefix := Some("org.vulpine_designs.initiator_set.map_aic"),
		libraryDependencies += scalactic,
		libraryDependencies += scalaTest % "test"
	)
lazy val lsd = (project in file("./LSD"))
	.dependsOn(utils)
	.settings(
		name := "LSD",
		idePackagePrefix := Some("org.vulpine_designs.initiator_set.lsd"),
		libraryDependencies += scalactic,
		libraryDependencies += scalaTest % "test"
	)
lazy val fasta = (project in file("./Fasta"))
	.dependsOn(utils)
	.settings(
		name := "Fasta",
		idePackagePrefix := Some("org.vulpine_designs.initiator_set.fasta"),
		libraryDependencies += scalactic,
		libraryDependencies += scalaTest % "test"
	)
