val commonSettings = Seq(
  scalaVersion := "3.8.2",
  scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xlint")
)

lazy val root = (project in file("."))
  .aggregate(exercises)
  .settings(commonSettings)
  .settings(
    name := "fp-in-scala",
    version := "0.1.0"
  )

lazy val exercises = (project in file("exercises"))
  .settings(commonSettings)
  .settings(
    name := "exercises"
  )
