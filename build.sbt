name := "JSON"

organization := "com.rayrobdod"

organizationHomepage := Some(new URL("http://rayrobdod.name/"))

version := "1.0.0-SNAPSHOT"

scalaVersion := "2.9.3"

crossScalaVersions ++= Seq("2.9.0", "2.9.1", "2.9.2", "2.10.2") //, "2.11.0-M4")

target := new File("C:/Users/Raymond/AppData/Local/Temp/build/JSON/")

compileOrder := CompileOrder.JavaThenScala

javacOptions ++= Seq("-Xlint:deprecation", "-Xlint:unchecked")

scalacOptions ++= Seq("-unchecked", "-deprecation" )

//scalacOptions in Doc <++= scalaVersion.map{(v:String) => 
//	Some(v).filter{_ == "2.10.0"}.map{(x:String) => "-implicits"}.toSeq
//}


scalaVersion in Test := "2.9.3"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

// testOptions in Test += Tests.Argument("-oS")
