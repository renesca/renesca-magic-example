name := "renesca-example"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  "com.github.renesca" %% "renesca" % "0.3.0",
  "com.github.renesca" %% "renesca-magic" % "0.3.0"
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
