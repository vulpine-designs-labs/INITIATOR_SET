
name := "initiator_set"

version := "0.0.3"

lazy val root = project.in(file("."))
	.aggregate(mainProject.js, mainProject.jvm)


lazy val mainProject = crossProject(JSPlatform, JVMPlatform).in(file("."))
	.settings(
		name := "initiator_set",
		idePackagePrefix := Some("org.vulpinedesigns.initiator_set"),
		scalaVersion := "3.1.3"
	)
	.jvmSettings(
		libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.15",
		libraryDependencies +="org.scalatest" %% "scalatest" % "3.2.15" % "test",
	)
	.jsSettings(
		libraryDependencies += "org.scalactic" %%% "scalactic" % "3.2.15",
		libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.15" % "test"
	)
