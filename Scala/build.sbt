name := "SCP"

assembly / assemblyMergeStrategy  := {
  case PathList("module-info.class") => MergeStrategy.discard
  case x if x.endsWith("/module-info.class") => MergeStrategy.discard
  case x =>
    val oldStrategy = (assembly / assemblyMergeStrategy).value
    oldStrategy(x)
}

version := "1.0"

scalaVersion := "2.13.1"
scalacOptions ++= Seq("-unchecked", "-deprecation")

lazy val akkaVersion = "2.6.18"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion,
  "com.typesafe.akka" %% "akka-cluster-typed"         % akkaVersion,
  "com.typesafe.akka" %% "akka-serialization-jackson" % akkaVersion,
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "io.aeron" % "aeron-driver" % "1.37.0",
  "io.aeron" % "aeron-client" % "1.37.0"
)