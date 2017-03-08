import Dependencies._

name := """fun-cqrs-raffle-demo"""

version := "1.0-SNAPSHOT"

scalaVersion in ThisBuild := "2.11.8"

libraryDependencies ++= Seq(ws) ++ appDeps ++ testDeps

lazy val app = (project in file(".")).enablePlugins(PlayScala)
