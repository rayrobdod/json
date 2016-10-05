// No support for 2.12
if (System.getProperty("scoverage.disable", "") != "true") {
  addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.3.5")
} else {
  TaskKey[Unit]("asfdsdfasdf") := {}
}

if (System.getProperty("scoverage.disable", "") != "true") {
  addSbtPlugin("org.scoverage" % "sbt-coveralls" % "1.1.0")
} else {
  TaskKey[Unit]("asfdsdfasdf") := {}
}

// only works with scala 2.11
// addSbtPlugin("com.sksamuel.scapegoat" %% "sbt-scapegoat" % "0.94.6")

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.8.0")
