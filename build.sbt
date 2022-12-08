val scala3Version = "3.2.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scalavantofcode2022",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.9.0"
  )
