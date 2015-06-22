name := "renesca-example"

scalaVersion := "2.11.6"

val paradiseVersion = "2.1.0-M5"

libraryDependencies ++= Seq(
  "com.github.renesca" %% "renesca" % "0.2.4",
  "com.github.renesca" %% "renesca-magic" % "0.1.8"
)

addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)
