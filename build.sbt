organization := "ru.vpavkin"
scalaVersion := "2.11.7"

lazy val shapelessVersion = "2.3.0"
lazy val catsVersion = "0.6.0"

lazy val root = project.in(file("."))
    .settings(
      libraryDependencies ++= Seq(
        "com.chuusai" %% "shapeless" % shapelessVersion,
        "org.typelevel" %% "cats" % catsVersion
      )
    )
