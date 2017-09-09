
val sharedSettings = Seq(
  scalaVersion := "2.12.3"
)
val agent = project
  .settings(
    sharedSettings,
    packageOptions in (Compile, packageBin) += 
     Package.ManifestAttributes( "Premain-Class" -> "agent.Agent" )
  )

val bench = project
  .dependsOn(agent)
  .settings(
    sharedSettings,
    fork in run := true,

    libraryDependencies += "com.lihaoyi" %% "upickle" % "0.4.4",
    libraryDependencies += "com.lihaoyi" %% "pprint" % "0.4.4",
    //libraryDependencies += "com.lihaoyi" % "ammonite" % "1.0.2",
    libraryDependencies += "com.lihaoyi" %% "ammonite-ops" % "1.0.2",
	//libraryDependencies += "com.lihaoyi" % "ammonite" % "1.0.2" % "test" cross CrossVersion.full,
    //libraryDependencies += "com.lihaoyi" % "ammonite_2.12.3" % "0.7.7",
    javaOptions in run += ("-javaagent:" + (packageBin in (agent, Compile)).value)
)
