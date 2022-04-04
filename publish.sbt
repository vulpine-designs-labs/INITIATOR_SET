ThisBuild / organization := "org.vulpinedesigns.initiator_set"
ThisBuild / organizationName := "vulpinedesigns"
ThisBuild / organizationHomepage := Some(url("https://www.vulpinedesigns.com"))

ThisBuild / scmInfo := Some(
	ScmInfo(
		url("https://gitlab.vulpinedesigns.com/TheGUESSUniversalEditingSuiteandSDK/initiator_set.scala"),
		"scm:git@gitlab.vulpinedesigns.com:9023/TheGUESSUniversalEditingSuiteandSDK/initiator_set.scala.git"
	)
)

ThisBuild / developers := List(
	Developer(
		id    = "jasonalexander-ja",
		name  = "Jason Alexander",
		email = "jasonalexander.dev@gmail.com",
		url   = url("https://www.vulpinedesigns.com")
	)
)

ThisBuild / description := "mRNA to protein translation variables calculator."
ThisBuild / licenses := List("GPL-2.0-only" -> new URL("https://opensource.org/licenses/gpl-2.0.php"))
ThisBuild / homepage := Some(url("https://www.vulpinedesigns.com/INITIATOR_SET"))

// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }

ThisBuild / publishTo := {
	val nexus = "https://s01.oss.sonatype.org/"
	if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
	else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

ThisBuild / publishMavenStyle := true

ThisBuild / versionScheme := Some("early-semver")