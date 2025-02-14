// See LICENSE.Berkeley for license details.

import sbt.complete.DefaultParsers._
import scala.sys.process._

enablePlugins(PackPlugin)

val chiselVersion = "3.5.6"
val circeVersion = "0.14.1"


lazy val commonSettings = Seq(
  organization := "edu.berkeley.cs",
  version      := "1.6.0",
  scalaVersion := "2.13.10",
  parallelExecution in Global := false,
  traceLevel   := 15,
  scalacOptions ++= Seq("-deprecation","-unchecked"),
  scalacOptions ++= Seq("-g:vars"),
  libraryDependencies ++= Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value),
  libraryDependencies ++= Seq("org.json4s" %% "json4s-jackson" % "3.6.6"),
  libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "3.2.0" % "test"),
  libraryDependencies ++= Seq("org.scalanlp" %% "breeze" % "2.1.0"),
  libraryDependencies ++= Seq(
    "io.circe" %% "circe-core",
    "io.circe" %% "circe-generic",
    "io.circe" %% "circe-parser"
  ).map(_ % circeVersion),
  resolvers ++= Seq(
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases"),
    Resolver.mavenLocal
  ),
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { x => false },
  pomExtra := <url>https://github.com/chipsalliance/rocket-chip</url>
  <licenses>
    <license>
      <name>Apache 2</name>
      <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
      <distribution>repo</distribution>
    </license>
    <license>
      <name>BSD-style</name>
        <url>http://www.opensource.org/licenses/bsd-license.php</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>https://github.com/chipsalliance/rocketchip.git</url>
      <connection>scm:git:github.com/chipsalliance/rocketchip.git</connection>
    </scm>,
  publishTo := {
    val v = version.value
    val nexus = "https://oss.sonatype.org/"
    if (v.trim.endsWith("SNAPSHOT")) {
      Some("snapshots" at nexus + "content/repositories/snapshots")
    }
    else {
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
    }
  }
)

lazy val chiselSettings = Seq(
  libraryDependencies ++= Seq("edu.berkeley.cs" %% "chisel3" % chiselVersion),
  libraryDependencies ++= Seq("edu.berkeley.cs" %% "chiseltest" % "0.5.2"),
  addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full)
)

lazy val cde = (project in file("cde"))
  .settings(commonSettings)
  .settings(publishArtifact := false)
  .settings(Compile / scalaSource := baseDirectory.value / "cde/src/chipsalliance/rocketchip")
lazy val hardfloat  = (project in file("hardfloat"))
  .settings(commonSettings, chiselSettings)
  .settings(publishArtifact := false)
lazy val `rocket-macros` = (project in file("macros")).settings(commonSettings)
  .settings(publishArtifact := false)
lazy val rocketchip = (project in file("."))
  .settings(commonSettings, chipSettings, chiselSettings)
  .dependsOn(cde)
  .dependsOn(hardfloat)
  .dependsOn(`rocket-macros`)
  .settings( // Assembly settings
    assembly / test := {},
    assembly / assemblyJarName := "rocketchip.jar",
    assembly / assemblyOutputPath := baseDirectory.value / "rocketchip.jar"
  )
  .settings( // Settings for scalafix
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    scalacOptions += "-Ywarn-unused"
  )

lazy val addons = settingKey[Seq[String]]("list of addons used for this build")
lazy val make = inputKey[Unit]("trigger backend-specific makefile command")
val setMake = NotSpace ~ ( Space ~> NotSpace )

lazy val chipSettings = Seq(
  addons := {
    val a = sys.env.getOrElse("ROCKETCHIP_ADDONS", "")
    println(s"Using addons: $a")
    a.split(" ")
  },
  unmanagedSourceDirectories in Compile ++= addons.value.map(baseDirectory.value / _ / "src/main/scala"),
  mainClass in (Compile, run) := Some("rocketchip.Generator"),
  make := {
    val jobs = java.lang.Runtime.getRuntime.availableProcessors
    val (makeDir, target) = setMake.parsed
    (run in Compile).evaluated
    s"make -C $makeDir  -j $jobs $target".!
  }
)

// mdoc documentation target
lazy val docs = project
  .in(file("docs-target"))
  .dependsOn(rocketchip)
  .enablePlugins(MdocPlugin)
  .settings(commonSettings)
  .settings(
      scalacOptions += "-language:reflectiveCalls",
      mdocIn := file("docs/src"),
      mdocOut := file("docs/generated"),
      mdocExtraArguments := Seq("--cwd", "docs"),
      mdocVariables := Map(
        // build dir for mdoc programs to dump temp files
        "BUILD_DIR" -> "docs-target"
      )
  )
