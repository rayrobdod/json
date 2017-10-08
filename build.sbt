name := "json"

organization := "com.rayrobdod"

organizationHomepage := Some(new URL("http://rayrobdod.name/"))

homepage := Some(new URL("http://rayrobdod.name/programming/libraries/java/json/"))

apiURL := Some(url(s"http://doc.rayrobdod.name/json/${version.value}/"))

version := "3.1.1-SNAPSHOT"

scalaVersion := "2.10.6"

crossScalaVersions := Seq("2.10.6", "2.11.11", "2.12.3")

resolvers += "Sonatype OSS Staging" at "https://oss.sonatype.org/content/repositories/staging/"

compileOrder := CompileOrder.JavaThenScala

javacOptions in Compile ++= Seq("-Xlint:deprecation", "-Xlint:unchecked", "-source", "1.7", "-target", "1.7")

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

scalacOptions ++= (scalaBinaryVersion.value match {
	case "2.10" => Seq("-target:jvm-1.7")
	case "2.11" => Seq("-target:jvm-1.7", "-Ywarn-unused-import", "-Ywarn-unused", "-Xlint:_", "-Xlint:-adapted-args", "-Xfuture", "-Xcheckinit")
	case "2.12" => Seq("-target:jvm-1.8", "-Ywarn-unused-import", "-Ywarn-unused", "-Xlint:_", "-Xlint:-adapted-args", "-Xfuture", "-Xcheckinit")
	case _ => Nil
})

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


//scapegoatVersion := "1.3.3"

// scalaTest
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

testOptions in Test += Tests.Argument("-oS", "-u", s"${crossTarget.value}/test-results-junit" /*, "-h", s"${crossTarget.value}/test-results-html" */)

// compile sample as part of test
val makeDocCompilable = taskKey[Seq[File]]("Create a scalac-compilable version of the example usage file")
makeDocCompilable in Test := {
	val outFile = (sourceManaged in Test).value / "parsingExample.scala"
	val inFile = (baseDirectory).value / "doc" / "parsingExample.scala"
	val inContents = IO.readLines(inFile)
	val outContents = Seq("package com.rayrobdod.json.doc", "object parsingExample {") ++ inContents ++ Seq("}")
	IO.writeLines(outFile, outContents)
	
	val outFile2 = (sourceManaged in Test).value / "serializeExample.scala"
	val inFile2 = (baseDirectory).value / "doc" / "serializeExample.scala"
	val inContents2 = IO.readLines(inFile2)
	val outContents2 = Seq("package com.rayrobdod.json.doc", "object serializeExample {") ++ inContents2 ++ Seq("}")
	IO.writeLines(outFile2, outContents2)
	Seq(outFile, outFile2)
}

sourceGenerators in Test += (makeDocCompilable in Test).taskValue

/// benchmarks
/*
enablePlugins(net.tixxit.sbt.benchmark.BenchmarkPlugin)

libraryDependencies += "org.mdedetrich" %% "scala-json-ast" % "1.0.0-M7" % "benchmark-precompile"
libraryDependencies += "org.spire-math" %% "jawn-parser" % "0.10.4" % "benchmark-precompile"
libraryDependencies += "org.spire-math" %% "jawn-ast" % "0.10.4" % "benchmark-precompile"
*/

mimaPreviousArtifacts := Set(organization.value %% name.value % "3.1")

