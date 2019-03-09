scalaVersion := "2.12.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
excludeFilter := HiddenFileFilter || "example*"
