import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._

object Workcraft extends Build {
  val repos = Seq("Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository", DefaultMavenRepository, "Workcraft Maven Repository" at "http://workcraft.org/repository/maven2")

  lazy val junit = "junit" % "junit" % "4.10" % "test"
  lazy val scalatest = "org.scalatest" %% "scalatest" % "1.9.1" % "test"
  lazy val junitInterface = "com.novocode" % "junit-interface" % "0.10-M2" % "test"

  lazy val workcraft = Project(id = "workcraft", base = file("Workcraft"))
  .settings (assemblySettings:_*)
  .aggregate (gui, pnplugin, lolaplugin, petrifyplugin, fsmplugin, dotplugin)
  .dependsOn (gui, pnplugin, lolaplugin, petrifyplugin, fsmplugin, dotplugin)

  lazy val util = Project(id = "util", base = file("Util"))

  lazy val depMan = Project (id = "depman", base = file ("DependencyManager")) 
  .settings (libraryDependencies := Seq("org.pcollections" % "pcollections" % "2.1.2", scalatest, junitInterface), resolvers := repos) 
  .dependsOn (util)

  lazy val scalautil = Project(id = "scalautil", base = file("ScalaUtil"))
  .settings (libraryDependencies := Seq("org.scalaz" %% "scalaz-core" % "6.0.3"))
  .dependsOn (util, depMan)

  lazy val tasks = Project(id = "tasks", base = file ("Tasks"))
  .dependsOn (scalautil)

  lazy val logger = Project(id = "logger", base = file ("Logger"))
  .dependsOn (scalautil)

  lazy val pluginManager = Project(id = "pluginmanager", base = file ("PluginManager"))
  .settings (libraryDependencies := Seq (scalatest, junit))
  .dependsOn (logger)

  lazy val core = Project(id = "core", base = file ("Core"))
  .dependsOn (tasks)

  lazy val graphics = Project(id = "graphics", base = file("Graphics"))
  .dependsOn (scalautil, booleanFormulae)

  lazy val booleanFormulae = Project (id = "booleanformulae", base = file ("BooleanFormulae"))
  .dependsOn (scalautil)
  .settings (libraryDependencies := Seq (junit))


  lazy val gui = Project(id = "gui", base = file ("Gui"))
  .settings (libraryDependencies := Seq ( "org.streum" %% "configrity-core" % "1.0.0", "org.apache.xmlgraphics" % "batik-svg-dom" % "1.7", "org.apache.xmlgraphics" % "batik-svggen" % "1.7",
                                          "org.apache.xmlgraphics" % "batik-bridge" % "1.7", "tablelayout" % "TableLayout" % "20050920", "org.flexdock" % "flexdock" % "1.2.2",
                                          "commons-logging" % "commons-logging" % "1.1", "com.github.insubstantial" % "substance" % "7.1", "com.github.insubstantial" % "trident" % "7.1", "com.google.guava" % "guava" % "11.0.2")
             , resolvers := repos)
  .dependsOn (core, graphics, logger)
  .settings (resourceDirectory in Compile <<= baseDirectory { _ / "src" / "main" / "resources" })
  .settings (scalacOptions ++= Seq("-unchecked", "-deprecation"))


  lazy val pnplugin = Project (id = "pnplugin", base = file ("PetriNetPlugin2"))
  .dependsOn (gui, graphedutil)

  lazy val graphedutil = Project (id = "graphedutil", base = file ("ScalaGraphEditorUtil"))
  .dependsOn (scalautil, graphics, gui)

  lazy val lolaplugin = Project(id = "lolaplugin", base = file ("LolaPlugin"))
  .dependsOn (pnplugin)

  lazy val petrifyplugin = Project (id = "petrifyplugin", base = file ("PetrifyPlugin2"))
  .dependsOn (pnplugin, fsmplugin, gui)

  lazy val dotplugin = Project (id = "dotplugin", base = file ("DotPlugin"))
  .dependsOn (gui)

  lazy val fsmplugin = 	Project (id = "fsmplugin", base = file ("FSMPlugin"))
  .dependsOn (gui, pnplugin, graphedutil)

  lazy val mailservice = Project (id = "mailservice", base = file ("MailService"))
  .settings (assemblySettings:_*)
  .settings (libraryDependencies := Seq ("javax.mail" % "mail" % "1.4.5", "com.google.guava" % "guava" % "11.0.2"))
  .dependsOn(core, pnplugin, lolaplugin, petrifyplugin)
}
