lazy val root = (project in file(".")).
  settings(scalariformSettings: _*).
  settings(
    name := "ond",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.11.7",
    libraryDependencies ++= Seq(
      "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
      "org.slf4j" % "slf4j-log4j12" % "1.7.13",
      "org.scalatest" %% "scalatest" % "2.2.4" % "test",
      "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"
  ))

scalacOptions ++= Seq("-unchecked", "-feature", "-deprecation" /*, "-Xprint:parser"*/)
