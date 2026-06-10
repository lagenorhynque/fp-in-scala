name := "fpinscala"

ThisBuild / scalaVersion := "3.8.4"

ThisBuild / githubWorkflowBuild := Seq(
  WorkflowStep.Sbt(
    name = Some("Build project"),
    commands = List("test:compile")
  )
)

ThisBuild / scalacOptions ++= List(
  "-feature",
  "-deprecation",
  "-Xkind-projector:underscores",
  "-source:future"
)

ThisBuild / libraryDependencies += "org.scalameta" %% "munit" % "1.3.3" % Test
