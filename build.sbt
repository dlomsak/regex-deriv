name := "regex-deriv"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
)

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-u", "target/test-reports")