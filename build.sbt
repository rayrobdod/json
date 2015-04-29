name := "JSON"

organization := "com.rayrobdod"

organizationHomepage := Some(new URL("http://rayrobdod.name/"))

version := "2.0-SNAPSHOT"

scalaVersion := "2.10.5"

crossScalaVersions := Seq("2.10.5", "2.11.6")

compileOrder := CompileOrder.JavaThenScala

javacOptions ++= Seq("-Xlint:deprecation", "-Xlint:unchecked")

scalacOptions ++= Seq("-unchecked", "-deprecation")

libraryDependencies <+= scalaVersion.apply{("org.scala-lang" % "scala-reflect" % _)}



artifact in (Compile, packageDoc) := {
	(artifact in (Compile, packageDoc)).value.copy(extension = "zip")
}

// TODO: tarball
artifact in (Compile, packageSrc) := {
	(artifact in (Compile, packageSrc)).value.copy(extension = "zip")
}

packageOptions in (Compile, packageBin) <+= (scalaVersion, sourceDirectory).map{(scalaVersion:String, srcDir:File) =>
	val manifest = new java.util.jar.Manifest(new java.io.FileInputStream(srcDir + "/main/MANIFEST.MF"))
	manifest.getAttributes("scala/").putValue("Implementation-Version", scalaVersion)
	Package.JarManifest( manifest )
}

licenses += (("3-point BSD", new java.net.URL("http://rayrobdod.name/programming/libraries/java/json/2.0.0/LICENSE.rst") ))

mappings in (Compile, packageSrc) <+= baseDirectory.map{(b) => (new File(b, "LICENSE.rst"), "LICENSE.rst" )}

mappings in (Compile, packageBin) <+= baseDirectory.map{(b) => (new File(b, "LICENSE.rst"), "LICENSE.rst" )}




// scalaTest

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.1" % "test"

testOptions in Test += Tests.Argument("-oS")
