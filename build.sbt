val scala3Version = "3.2.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scalavantofcode2022",

    run / fork := true,

    scalaVersion := scala3Version,

    libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.9.0"
  )
