import sbt._
import Keys._

object PropTestProject extends Build
{
  import Resolvers._
  lazy val root = Project("PropTest", file(".")) settings(coreSettings : _*)

  lazy val commonSettings: Seq[Setting[_]] = Seq(
    organization := "net.debasishg",
    version := "0.01",
    scalaVersion := "2.11.1",
    crossScalaVersions := Seq("2.11.1"),

    scalacOptions in Compile ++= Seq( "-unchecked", "-feature", "-language:postfixOps", "-deprecation" )
  )

  lazy val coreSettings = commonSettings ++ Seq(
    name := "PropTest",
    libraryDependencies := Seq(
      "org.scalaz"        %% "scalaz-core"               % "7.1.0",
      "joda-time"          % "joda-time"                 % "2.1",
      "org.joda"           % "joda-convert"              % "1.3",
      "org.scalacheck"    %% "scalacheck"                % "1.11.5"      % "test",
      "org.scalaz"        %% "scalaz-scalacheck-binding" % "7.1.0"       % "test",
      "org.specs2"        %% "specs2"                    % "2.4.2"       % "test",
      "org.typelevel"     %% "scalaz-specs2"             % "0.3.0"       % "test"
    ),
    parallelExecution in Test := false,
    publishTo <<= version { (v: String) => 
      val nexus = "https://oss.sonatype.org/" 
      if (v.trim.endsWith("SNAPSHOT")) Some("snapshots" at nexus + "content/repositories/snapshots")
      else Some("releases" at nexus + "service/local/staging/deploy/maven2") 
    },
    credentials += Credentials(Path.userHome / ".sbt" / "sonatype.credentials"),
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := { repo => false },
    pomExtra := (
      <url>https://github.com/debasishg/proptest</url>
      <licenses>
        <license>
          <name>Apache 2.0 License</name>
          <url>http://www.apache.org/licenses/LICENSE-2.0.html</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git@github.com:debasishg/proptest.git</url>
        <connection>scm:git:git@github.com:debasishg/proptest.git</connection>
      </scm>
      <developers>
        <developer>
          <id>debasishg</id>
          <name>Debasish Ghosh</name>
          <url>http://debasishg.blogspot.com</url>
        </developer>
      </developers>),
    unmanagedResources in Compile <+= baseDirectory map { _ / "LICENSE" }
  )
}
