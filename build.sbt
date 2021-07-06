
lazy val root = (project in file("."))
  .settings(
    inThisBuild(List(
      organization := "com.mercerenies.befreak",
      scalaVersion := "3.0.0"
    )),
    name := "stacked"
  )

Compile / unmanagedSourceDirectories += baseDirectory.value / "src"
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.4.0-M7"
scalacOptions += "-Yexplicit-nulls"
