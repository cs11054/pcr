name := "mods"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  "com.typesafe.slick" %% "slick" % "1.0.1",
  "com.typesafe.play" %% "play-slick" % "0.5.0.8",
  "com.h2database" % "h2" % "1.3.166",
  cache
)     

scalaVersion := "2.10.4"

play.Project.playScalaSettings
