name := "JSON"

organization := "com.rayrobdod"

organizationHomepage := Some(new URL("http://rayrobdod.name/"))

version := "1.0.1-SNAPSHOT"

scalaVersion := "2.10.4"

crossScalaVersions ++= Seq("2.9.0", "2.9.1", "2.9.2", "2.9.3", "2.10.4", "2.11.4")

compileOrder := CompileOrder.JavaThenScala

javacOptions ++= Seq("-Xlint:deprecation", "-Xlint:unchecked")

scalacOptions ++= Seq("-unchecked", "-deprecation" )

//scalacOptions in Doc <++= scalaVersion.map{(v:String) => 
//	Some(v).filter{_ == "2.10.0"}.map{(x:String) => "-implicits"}.toSeq
//}


// scalaTest
scalaVersion in Test := "2.10.4"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.2.1" % "test"

testOptions in Test += Tests.Argument("-oS")
