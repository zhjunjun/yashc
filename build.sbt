name := "yashc"

version := "0.1"

scalaVersion := "2.13.3"

libraryDependencies += "org.asynchttpclient" % "async-http-client" % "2.10.4"

enablePlugins(BuildInfoPlugin)

buildInfoPackage := "yashc"