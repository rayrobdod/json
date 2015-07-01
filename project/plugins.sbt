resolvers += Classpaths.sbtPluginReleases

resolvers += "sonatype-releases" at "https://oss.sonatype.org/content/repositories/releases"

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.0.4")

addSbtPlugin("org.scoverage" % "sbt-coveralls" % "1.0.0")

// only works with scala 2.11
// addSbtPlugin("com.sksamuel.scapegoat" %% "sbt-scapegoat" % "0.94.6")

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.7.0")
