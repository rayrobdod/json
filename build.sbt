name := "JSON"

organization := "com.rayrobdod"

organizationHomepage := Some(new URL("http://rayrobdod.name/"))

version := "1.0.0"

scalaVersion := "2.9.3"

crossScalaVersions ++= Seq("2.9.0", "2.9.1", "2.9.2", "2.10.2", "2.11.0-M4")

target := new File("C:/Users/Raymond/AppData/Local/Temp/build/JSON/")

unmanagedSourceDirectories in Compile ++= Seq(
		new File("C:/Users/Raymond/Documents/Programming/Java/Utilities/")
)

includeFilter in Compile ~= (_ || new FileFilter{
	def accept(n:File) = {
		val abPath = n.getAbsolutePath().replace('\\', '/')
		(
		//	(abPath endsWith "com/rayrobdod/util/WrappedObject.java") ||
			((abPath contains "javaScriptObjectNotation") && ((abPath endsWith ".java") || (abPath endsWith ".scala"))) ||
			((abPath contains "binaryJSON") && ((abPath endsWith ".java") || (abPath endsWith ".scala")))
		)
	}
})

compileOrder := CompileOrder.JavaThenScala

javacOptions ++= Seq("-Xlint:deprecation", "-Xlint:unchecked")

scalacOptions ++= Seq("-unchecked", "-deprecation" )

//scalacOptions in Doc <++= scalaVersion.map{(v:String) => 
//	Some(v).filter{_ == "2.10.0"}.map{(x:String) => "-implicits"}.toSeq
//}


scalaVersion in Test := "2.9.3"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

// testOptions in Test += Tests.Argument("-oS")
