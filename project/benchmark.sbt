addSbtPlugin("net.tixxit" %% "sbt-benchmark" % "0.1.1")

libraryDependencies ++= Seq(
  "org.openjdk.jmh" % "jmh-core" % "1.0",
  "org.openjdk.jmh" % "jmh-generator-bytecode" % "1.0"
)
