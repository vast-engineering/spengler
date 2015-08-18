organization := "com.vast"

name := "spengler"

scalaVersion := "2.11.7"

crossScalaVersions := Seq("2.11.7")

libraryDependencies ++=
  Seq(
    "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
    "org.slf4j" % "slf4j-api" % "1.7.12",
    "com.fasterxml" % "aalto-xml" % "0.9.9",
    "org.scala-stm" %% "scala-stm" % "0.7",
    "org.scalatest" %% "scalatest" % "2.2.5" % "test",
    "ch.qos.logback" % "logback-classic" % "1.1.3" % "test",
    "commons-io" % "commons-io" % "2.4" % "test"
  )

// add scala-xml dependency when needed (for Scala 2.11 and newer) in a robust way
// this mechanism supports cross-version publishing
// taken from: http://github.com/scala/scala-module-dependency-sample
libraryDependencies ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    // if scala 2.11+ is used, add dependency on scala-xml module
    case Some((2, scalaMajor)) if scalaMajor >= 11 =>
      Seq("org.scala-lang.modules" %% "scala-xml" % "1.0.2")
    case _ =>
      Seq.empty
  }
}

releaseSettings
