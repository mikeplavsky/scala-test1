name := "hello"

version := "1.0"

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.10.1" % "test", 
  "org.apache.httpcomponents" % "httpclient" % "4.2.5",
  "org.specs2" % "specs2_2.10" % "2.0",
  "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
)
