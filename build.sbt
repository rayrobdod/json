name := "json"

organization := "com.rayrobdod"

organizationHomepage := Some(new URL("http://rayrobdod.name/"))

homepage := Some(new URL("http://rayrobdod.name/programming/libraries/java/json/"))

apiURL := Some(url(s"http://doc.rayrobdod.name/json/${version.value}/"))

version := "3.0-SNAPSHOT"

scalaVersion := "2.10.6"

crossScalaVersions := Seq("2.10.6", "2.11.8") ++
    (if (System.getProperty("scoverage.disable", "") != "true") {Nil} else {Seq("2.12.0")})

compileOrder := CompileOrder.JavaThenScala

javacOptions in Compile ++= Seq("-Xlint:deprecation", "-Xlint:unchecked", "-source", "1.7", "-target", "1.7")

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-target:jvm-1.7")

scalacOptions ++= (if (scalaVersion.value != "2.11.8") {Nil} else {Seq("-Ywarn-unused-import", "-Ywarn-unused", "-Xlint:_", "-Xlint:-adapted-args")})

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

scalacOptions in doc in Compile ++= Seq(
		"-doc-title", name.value,
		"-doc-version", version.value,
		"-doc-root-content", ((scalaSource in Compile).value / "rootdoc.txt").toString,
		"-diagrams",
		"-sourcepath", baseDirectory.value.toString,
		"-doc-source-url", "https://github.com/rayrobdod/json/tree/" + version.value + "€{FILE_PATH}.scala"
)

autoAPIMappings in doc in Compile := true

packageOptions in (Compile, packageBin) += {
	val manifest = new java.util.jar.Manifest()
	manifest.getEntries().put("scala/", {
		val attrs = new java.util.jar.Attributes()
		attrs.putValue("Implementation-Title", "Scala")
		attrs.putValue("Implementation-URL", "http://www.scala-lang.org/")
		attrs.putValue("Implementation-Version", scalaVersion.value)
		attrs
	})
	Package.JarManifest( manifest )
}

licenses += (("3-point BSD", new URL("http://opensource.org/licenses/BSD-3-Clause") ))

val readableNoteMappings = Def.task{ Seq(
	baseDirectory.value / "LICENSE.rst" -> "LICENSE.rst",
	baseDirectory.value / "CHANGES.md" -> "CHANGES.md"
)}
mappings in (Compile, packageSrc) ++= readableNoteMappings.value
mappings in (Compile, packageBin) ++= readableNoteMappings.value

scalastyleConfig := baseDirectory.value / "project" / "scalastyle-config.xml"


if (System.getProperty("scoverage.disable", "") == "true") {
	// provide no-op replacements for disabled tasks
	TaskKey[Unit]("coverage") := {}
} else {
	TaskKey[Unit]("asfdsdfasdf") := {}
}

if (System.getProperty("scoverage.disable", "") == "true") {
	// provide no-op replacements for disabled tasks
	TaskKey[Unit]("coveralls") := {}
} else {
	TaskKey[Unit]("asfdsdfasdf") := {}
}

if (System.getProperty("scoverage.disable", "") == "true") {
	// provide no-op replacements for disabled tasks
	TaskKey[Unit]("coverageReport") := {}
} else {
	TaskKey[Unit]("asfdsdfasdf") := {}
}


// scalaTest
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

testOptions in Test += Tests.Argument("-oS", "-u", s"${crossTarget.value}/test-results-junit" /*, "-h", s"${crossTarget.value}/test-results-html" */)

// compile sample as part of test
val makeDocCompilable = taskKey[Seq[File]]("Create a scalac-compilable version of the example usage file")
makeDocCompilable in Test := {
	val outFile = (sourceManaged in Test).value / "exampleUsage.scala"
	val inFile = (baseDirectory).value / "doc" / "exampleUsage.scala"
	val inContents = IO.readLines(inFile)
	val outContents = Seq("package com.rayrobdod.json.doc", "object exampleUsage {") ++ inContents ++ Seq("}")
	IO.writeLines(outFile, outContents)
	Seq(outFile)
}

sourceGenerators in Test += (makeDocCompilable in Test).taskValue
