name := "links-collector"

scalaVersion := "2.10.2"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.2.3",
  "com.typesafe.akka" %% "akka-testkit" % "2.2.3",
  "com.ning" % "async-http-client" % "1.7.19"
)

mainClass in (Compile, run) := Some("akka.Main")