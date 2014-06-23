organization := "com.vast"

name := "spengler"

scalaVersion := "2.10.4"

crossScalaVersions := Seq("2.10.4", "2.11.1")

libraryDependencies ++=
  Seq(
    "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2",
    "org.slf4j" % "slf4j-api" % "1.7.7",
	"com.fasterxml" % "aalto-xml" % "0.9.9",
	"org.scala-stm" %% "scala-stm" % "0.7",
    "org.scalatest" %% "scalatest" % "2.2.0" % "test",
    "ch.qos.logback" % "logback-classic" % "1.1.2" % "test",
	"commons-io" % "commons-io" % "2.4" % "test"
  )

// add scala-xml dependency when needed (for Scala 2.11 and newer) in a robust way
// this mechanism supports cross-version publishing
// taken from: http://github.com/scala/scala-module-dependency-sample
libraryDependencies ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    // if scala 2.11+ is used, add dependency on scala-xml module
    case Some((2, scalaMajor)) if scalaMajor >= 11 =>
      Seq("org.scala-lang.modules" %% "scala-xml" % "1.0.1")
    case _ =>
	  Seq.empty
  }
}

packageBin in Compile <<= (packageBin in Compile).dependsOn(test in Test)

releaseSettings