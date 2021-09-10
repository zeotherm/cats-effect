val scala3Version = "3.0.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "cats-effect",
    version := "0.1.0",

    scalaVersion :=  scala3Version, //scala3Version, "2.13.6",

    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.2.0",
    )
  )
