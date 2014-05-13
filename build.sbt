name := "daffodil"

organization in ThisBuild := "edu.illinois.ncsa"

scalaVersion in ThisBuild := "2.11.0"

scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation", "-Xfatal-warnings")

parallelExecution in ThisBuild := false

logBuffered in ThisBuild := false

testOptions in ThisBuild += Tests.Argument(TestFrameworks.JUnit, "-v")

transitiveClassifiers := Seq("sources", "javadoc")

resolvers in ThisBuild += "NCSA Sonatype Releases" at "https://opensource.ncsa.illinois.edu/nexus/content/repositories/releases"

libraryDependencies in ThisBuild := Seq(
  "org.scala-lang.modules" %% "scala-xml" % "1.0.1",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1",
  "org.scala-lang" % "scala-actors" % "2.11.0",
  "junit" % "junit" % "4.11",
  "com.novocode" % "junit-interface" % "0.10-M4",
  "org.jdom" % "jdom2" % "2.0.5",
  "net.sf.saxon" % "Saxon-HE" % "9.5.1-1",
  "net.sf.saxon" % "Saxon-HE-jdom2" % "9.5.1-1",
  "com.ibm.icu" % "icu4j" % "51.1",// classifier "" classifier "charset" classifier "localespi",
  "xerces" % "xercesImpl" % "2.10.0",
  "xml-resolver" % "xml-resolver" % "1.2",
  "jline" % "jline" % "2.9",
  "org.rogach" %% "scallop" % "0.9.5",
  "net.sourceforge.expectj" % "expectj" % "2.0.7",
  "commons-io" % "commons-io" % "2.4"
)

retrieveManaged := true

exportJars in ThisBuild := true

exportJars in Test in ThisBuild := false





publishMavenStyle in ThisBuild := true

publishArtifact in Test := false

publishTo in ThisBuild <<= version { (v: String) =>
  val nexus = "https://opensource.ncsa.illinois.edu/nexus/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("NCSA Sonatype Nexus Snapshot" at nexus + "content/repositories/snapshots")
  else
    Some("NCSA Sonatype Nexus Release" at nexus + "content/repositories/releases")
}

pomIncludeRepository in ThisBuild := { _ => false }

pomExtra in ThisBuild := (
  <scm>
    <url>https://opensource.ncsa.illinois.edu/fisheye/changelog/dfdl</url>
    <connection>scm:git:https://opensource.ncsa.illinois.edu/fisheye/git/dfdl.git</connection>
  </scm>
  <developers>
    <developer>
      <id>Tresys</id>
      <name>Tresys</name>
      <url>http://www.tresys.com</url>
    </developer>
  </developers>
)

licenses := Seq("University of Illinois/NCSA Open Source License" -> url("http://opensource.org/licenses/UoI-NCSA.php"))

homepage := Some(url("https://opensource.ncsa.illinois.edu/confluence/display/DFDL/Home"))
