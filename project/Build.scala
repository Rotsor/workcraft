import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._

object Workcraft extends Build {
    
    val rootTargetDirectory = file("target")
    
    object Utils {
      val repos = Seq("Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository", DefaultMavenRepository, "Workcraft Maven Repository" at "http://workcraft.org/repository/maven2")

      sealed trait Lang
      case object Java extends Lang
      case object Scala extends Lang

      sealed trait ProjectType
      case class SimpleProject(lang : Lang) extends ProjectType
      case object MixedProject extends ProjectType

      def camelToWords = {
        def go(w : List[Char]) : List[Char] => List[List[Char]] = {
          case Nil => List(w.reverse)
          case (c :: cs) => if(c.isUpper) w.reverse :: go(List(c.toLower))(cs) else go(c :: w)(cs)
        }
        go(Nil)
      }
      def camelToDashed(s : String) = camelToWords(s.toList).filter(x => !x.isEmpty).map(_.mkString).mkString("-")

      def promoteLang : ProjectType => List[Setting[_]] = {
	case SimpleProject(l) => List(Compile, Test).map { phase =>
          (l match {case Java => javaSource; case Scala => scalaSource}) in phase <<= sourceDirectory in phase
							}
	case MixedProject => List(Compile, Test).flatMap { phase => 
	  List(javaSource <<= (sourceDirectory in phase) (_ / "java")
	       , scalaSource <<= (sourceDirectory in phase) (_ / "scala")
	       , resourceDirectory <<= (sourceDirectory in phase) (_ / "resources")
	       )}
      }

      def MyProject(name : String, typ : ProjectType) = Project(id = camelToDashed(name), base = file(name))
       .settings(resolvers := repos)
       .settings(promoteLang(typ):_*)
       .settings(target := rootTargetDirectory / name)
       .settings(sourceDirectory <<= baseDirectory)
       .settings (scalacOptions ++= Seq("-unchecked", "-deprecation"))

      def simpleProject(name : String, lang : Lang) = MyProject(name, SimpleProject(lang))
      def mixedProject(name : String) = MyProject(name, MixedProject)
    }
    import Utils._

    lazy val junit_always = "junit" % "junit" % "4.10"
    lazy val junit = "junit" % "junit" % "4.10" % "test"
    lazy val scalatest = "org.scalatest" %% "scalatest" % "1.9.1" % "test"
    lazy val junitInterface = "com.novocode" % "junit-interface" % "0.10-M2" % "test"
    lazy val pcollections = "org.pcollections" % "pcollections" % "2.1.2"
    lazy val scalaz = "org.scalaz" %% "scalaz-core" % "6.0.3"
    lazy val configrity = "org.streum" %% "configrity-core" % "1.0.0"
    lazy val batikSvg = Seq(
      "org.apache.xmlgraphics" % "batik-svg-dom" % "1.7",
      "org.apache.xmlgraphics" % "batik-svggen" % "1.7",
      "org.apache.xmlgraphics" % "batik-bridge" % "1.7")
    lazy val tableLayout = "tablelayout" % "TableLayout" % "20050920"
    lazy val flexDock = "org.flexdock" % "flexdock" % "1.2.2"
    lazy val logging = "commons-logging" % "commons-logging" % "1.1"
    lazy val substance = Seq("com.github.insubstantial" % "substance" % "7.1", 
         "com.github.insubstantial" % "trident" % "7.1")
    lazy val guava = "com.google.guava" % "guava" % "11.0.2"
    lazy val mail = "javax.mail" % "mail" % "1.4.5"

    lazy val util = simpleProject ("Util", Java)

    lazy val depMan = simpleProject ("DependencyManager", Java)
     .settings (libraryDependencies := Seq(pcollections, junitInterface))
     .settings (parallelExecution in Test := false)
     .dependsOn (util)

    lazy val scalautil = simpleProject("ScalaUtil", Scala)
     .settings (libraryDependencies := Seq(scalaz))
     .dependsOn (util, depMan)

    lazy val tasks = simpleProject("Tasks", Scala)
     .dependsOn (scalautil)

    lazy val logger = simpleProject("Logger", Scala)
     .dependsOn (scalautil)

    lazy val pluginManager = mixedProject("PluginManager")
     .settings (libraryDependencies := Seq (scalatest, junit))
     .dependsOn (logger)

    lazy val core = simpleProject("Core", Scala)
     .dependsOn (tasks)

    lazy val booleanFormulae = simpleProject("BooleanFormulae", Java)
     .dependsOn (util)
     .settings (libraryDependencies := Seq (junit_always))

    lazy val graphics = mixedProject("Graphics")
     .dependsOn (scalautil, booleanFormulae)

    lazy val gui = mixedProject("Gui")
     .settings (libraryDependencies := Seq (configrity) ++ batikSvg ++ Seq(tableLayout, flexDock) ++ substance ++ Seq(guava))
     .dependsOn (core, graphics, logger)


    lazy val pnplugin = simpleProject ("PetriNetPlugin2", Scala)
     .dependsOn (gui, graphedutil)

    lazy val graphedutil = simpleProject ("ScalaGraphEditorUtil", Scala)
     .dependsOn (scalautil, graphics, gui)

    lazy val lolaplugin = simpleProject("LolaPlugin", Scala)
     .dependsOn (pnplugin)

    lazy val petrifyplugin = simpleProject ("PetrifyPlugin2", Scala)
     .dependsOn (pnplugin, fsmplugin, gui)

    lazy val dotplugin = simpleProject ("DotPlugin", Scala)
     .dependsOn (gui)

    lazy val fsmplugin = simpleProject ("FSMPlugin", Scala)
     .dependsOn (gui, pnplugin, graphedutil)

    lazy val mailservice = simpleProject ("MailService", Scala)
     .settings (assemblySettings:_*)
     .settings (libraryDependencies := Seq (mail, guava))
     .dependsOn(core, pnplugin, lolaplugin, petrifyplugin)

    lazy val workcraft = mixedProject("Workcraft")
     .settings (assemblySettings:_*)
     .aggregate (gui, pnplugin, lolaplugin, petrifyplugin, fsmplugin, dotplugin)
     .dependsOn (gui, pnplugin, lolaplugin, petrifyplugin, fsmplugin, dotplugin)
}
