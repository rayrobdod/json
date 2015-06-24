name := "json"

organization := "com.rayrobdod"

organizationHomepage := Some(new URL("http://rayrobdod.name/"))

version := "2.0-SNAPSHOT"

scalaVersion := "2.10.5"

crossScalaVersions := Seq("2.10.5", "2.11.6")

compileOrder := CompileOrder.JavaThenScala

javacOptions ++= Seq("-Xlint:deprecation", "-Xlint:unchecked")

scalacOptions ++= Seq("-unchecked", "-deprecation")

libraryDependencies <+= scalaVersion.apply{("org.scala-lang" % "scala-reflect" % _)}


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
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.1" % "test"

testOptions in Test += Tests.Argument("-oS")
