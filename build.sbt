scalaVersion := "2.13.3"

libraryDependencies += "org.scalatest" % "scalatest_2.13" % "3.0.8" % "test"

libraryDependencies += "com.lihaoyi" %% "pprint" % "0.5.6"

scalaSource in Compile := baseDirectory.value / "src"

scalaSource in Test := baseDirectory.value / "test" / "src"

artifactName in (Compile, packageSrc) := { (sv: ScalaVersion, module: ModuleID, artifact: Artifact) =>
    "for-marmoset.zip"
}

mappings in (Compile, packageSrc) := {
          (sources in Compile).value pair Path.rebase(baseDirectory.value / "src", "src/")
}

mappings in (Compile, packageSrc) ++= {
          ((baseDirectory.value / "test") ** "*") pair Path.rebase(baseDirectory.value / "test", "test/")
}
