import sbt._

class ScalaTypeAnxiety(info: ProjectInfo)
extends DefaultProject(info) with AutoCompilerPlugins
{
  val cont = compilerPlugin("org.scala-lang.plugins" % "continuations" %
                            "2.8.0.RC7")
  override def compileOptions =
    super.compileOptions ++ compileOptions("-P:continuations:enable")

  override def mainScalaSourcePath = "src"
}
