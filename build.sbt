lazy val root = (project in file(".")).
  settings(
    name := "GuideDog",
    version := "0.1",
    scalaVersion := "2.11.7",
    organization := "com.guidedog",
    javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),
    scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

  ).
  settings(
    libraryDependencies ++= Seq(
      "org.scalaz" %% "scalaz-core" % "7.1.4",
      "com.google.maps" % "google-maps-services" % "0.1.8",
      "io.spray" %% "spray-can" % "1.3.3",
      "io.spray" %% "spray-routing" % "1.3.3",
      "com.typesafe.akka" %% "akka-actor" % "2.4.0",
      "com.typesafe.akka" %% "akka-agent" % "2.4.0",
      "org.jsoup" % "jsoup" % "1.8.3"
    )
  )

