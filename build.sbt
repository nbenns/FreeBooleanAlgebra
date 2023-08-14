name := "FreeBooleanAlgebra"

version := "0.1"

scalaVersion := "3.3.0"

//addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)

scalacOptions ++= Seq(
  "-feature",
  "-Ykind-projector:underscores",
  "-source:future",
  "-deprecation"
)
