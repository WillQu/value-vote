enablePlugins(ScalaJSPlugin)

name := "Value Vote"
scalaVersion := "3.0.2" // or any other Scala version >= 2.11.12

// This is an application with a main method
scalaJSUseMainModuleInitializer := true

libraryDependencies += "com.raquo" %%% "laminar" % "0.13.1"
