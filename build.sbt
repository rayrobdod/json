name := "json"

organization := "com.rayrobdod"

organizationHomepage := Some(new URL("http://rayrobdod.name/"))

homepage := Some(new URL("http://rayrobdod.name/programming/libraries/java/json/"))

apiURL := Some(url(s"http://doc.rayrobdod.name/json/${version.value}/"))

version := "2.0-SNAPSHOT"

scalaVersion := "2.10.6"

crossScalaVersions := Seq("2.10.6", "2.11.7") ++
    (if (System.getProperty("scoverage.disable", "") != "true") {Nil} else {Seq("2.12.0-M3")})

compileOrder := CompileOrder.JavaThenScala

javacOptions in Compile ++= Seq("-Xlint:deprecation", "-Xlint:unchecked")

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies <+= scalaVersion.apply{("org.scala-lang" % "scala-reflect" % _)}

scalacOptions in doc in Compile ++= Seq(
		"-doc-title", name.value,
		"-doc-version", version.value,
		"-doc-root-content", ((scalaSource in Compile).value / "rootdoc.txt").toString,
		"-diagrams",
		"-sourcepath", baseDirectory.value.toString,
		"-doc-source-url", "https://github.com/rayrobdod/json/tree/" + version.value + "€{FILE_PATH}.scala"
)

autoAPIMappings in doc in Compile := true

packageOptions in (Compile, packageBin) <+= (scalaVersion, sourceDirectory).map{(scalaVersion:String, srcDir:File) =>
	val manifest = new java.util.jar.Manifest(new java.io.FileInputStream(srcDir + "/main/MANIFEST.MF"))
	manifest.getAttributes("scala/").putValue("Implementation-Version", scalaVersion)
	Package.JarManifest( manifest )
}

licenses += (("3-point BSD", new URL("http://opensource.org/licenses/BSD-3-Clause") ))

mappings in (Compile, packageSrc) <+= baseDirectory.map{(b) => (new File(b, "LICENSE.rst"), "LICENSE.rst" )}

mappings in (Compile, packageBin) <+= baseDirectory.map{(b) => (new File(b, "LICENSE.rst"), "LICENSE.rst" )}

mappings in (Compile, packageSrc) <+= baseDirectory.map{(b) => (new File(b, "CHANGES.md"), "CHANGES.md" )}

mappings in (Compile, packageBin) <+= baseDirectory.map{(b) => (new File(b, "CHANGES.md"), "CHANGES.md" )}

scalastyleConfig := baseDirectory.value / "project" / "scalastyle-config.xml"



// scalaTest
libraryDependencies += "org.scalatest" %% "scalatest" % (
      "2.2.5" + (if ((scalaVersion.value take 7) == "2.12.0-") { "-" + (scalaVersion.value drop 7) } else {""}) 
    ) % "test"

testOptions in Test += Tests.Argument("-oS")
