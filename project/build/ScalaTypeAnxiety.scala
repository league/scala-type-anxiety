import sbt._

class ScalaTypeAnxiety(info: ProjectInfo)
extends DefaultProject(info) with AutoCompilerPlugins
{
  val cont = compilerPlugin("org.scala-lang.plugins" % "continuations" %
                            "2.8.0.RC7")
  override def compileOptions =
    super.compileOptions ++ compileOptions("-P:continuations:enable")

  val scalaToolsSnapshots = "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"
  val scalazCore = "com.googlecode.scalaz" %% "scalaz-core" % "5.0-M3-SNAPSHOT"

  override def mainScalaSourcePath = "src"
}
