import com.typesafe.sbt.SbtStartScript

seq(SbtStartScript.startScriptForClassesSettings: _*)

name := "curt-api"

version := "1.0"

scalaVersion := "2.9.2"

resolvers ++= Seq(
	"twitter-repo" at "http://maven.twttr.com",
	"Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"
)

libraryDependencies ++= Seq(
	"org.scala-lang" % "scala-library" % "2.9.2",
	"org.scalatest" % "scalatest_2.9.2" % "1.7.2",
	"com.twitter" % "finatra" % "1.2.0",
	"org.squeryl" %% "squeryl" % "0.9.5-2", // Squeryl for db ORM http://squeryl.org
	"c3p0" % "c3p0" % "0.9.1.2", // c3p0 for db connection pooling http://www.mchange.com/projects/c3p0
	"com.typesafe.akka" % "akka-actor" % "2.0.5"
	)