name := "JSON"

organization := "com.rayrobdod"

organizationHomepage := Some(new URL("http://rayrobdod.name/"))

version := "2.0-SNAPSHOT"

scalaVersion := "2.10.5"

crossScalaVersions ++= Seq("2.10.5", "2.11.6")

compileOrder := CompileOrder.JavaThenScala

javacOptions ++= Seq("-Xlint:deprecation", "-Xlint:unchecked")

scalacOptions ++= Seq("-unchecked", "-deprecation")



// scalaTest
scalaVersion in Test := "2.10.5"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.2.1" % "test"

testOptions in Test += Tests.Argument("-oS")
