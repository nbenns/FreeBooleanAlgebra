name := "FreeBooleanAlgebra"

version := "0.1"

scalaVersion := "2.12.4"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8")

scalacOptions ++= Seq(
  "-Ypartial-unification",
  "-feature"
)