name := "RMapViewerS"

version := "1.0"

incOptions := incOptions.value.withNameHashing(true)

libraryDependencies ++= Seq(
  "org.json4s" %% "json4s-jackson" % "3.2.10",
  "org.scala-lang" % "scala-swing" % "2.10+"
)

libraryDependencies += "org.skinny-framework" %% "skinny-http-client" % "2.5.2"
libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.6.4"

TaskKey[Seq[java.io.File]]("collect-jars") <<=
  ( dependencyClasspath in Compile ) map { paths =>
    paths.map { path =>
      val jar = path.data
      val dist = new File("target/lib/"+jar.getName)
      org.apache.ivy.util.FileUtil.copy(jar,dist,null)
      dist
    }
  }
