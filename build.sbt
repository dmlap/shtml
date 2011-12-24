sbtPlugin := true

name := "shtml"

organization := "com.github.dmlap"

scalaVersion := "2.9.1"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "1.6.1" % "test",
  "org.specs2" %% "specs2-scalaz-core" % "6.0.1" % "test",
  "org.jsoup" % "jsoup" % "1.6.1"
)

resolvers ++= Seq("snapshots" at "http://scala-tools.org/repo-snapshots",
                  "releases"  at "http://scala-tools.org/repo-releases")
