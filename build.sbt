name := "renesca-example"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "com.github.renesca" %% "renesca-magic" % "0.3.2-1"
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
