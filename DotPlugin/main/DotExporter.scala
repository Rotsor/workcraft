package org.workcraft.plugins.dot
import java.io.File
import java.io.PrintStream
import java.io.BufferedOutputStream
import java.io.FileOutputStream
import scalaz.effect.IO
import scalaz.effect.IO._
import org.workcraft.services.layout._
import org.workcraft.services._
import scalaz.Scalaz._

/*object DotExporter extends Exporter {
  val targetFormat = Format.Dot

  def export(model: ModelServiceProvider): Either[ServiceNotAvailableException, ExportJob] = model.implementation(LayoutService) match {
    case Some(impl) => Right(new DotExportJob(impl ))
    case None => Left(new ServiceNotAvailableException(LayoutService))
  }
}*/

class DotExportJob[N](layout: LayoutSpec[N]) extends ExportJob {
  def job(file: File) = IO {
    val out = new PrintStream(new BufferedOutputStream(new FileOutputStream(file)))
    try {
      out.println("digraph work {");
      out.println("graph [nodesep=\"" + layout.nodeSeparation + "\", ranksep=\"" + layout.rankSeparation + "\", overlap=\"false\", splines=\"true\"];");
      out.println("node [shape=box];");

      val nodeToId = layout.nodes.zipWithIndex.toMap
      val idToNode = nodeToId.map(_.swap).toMap

      layout.nodes.foreach(node => {
        val (width, height) = layout.size(node)
        
        out.println("\"" + nodeToId(node) + "\" [width=\"" + width + "\", height=\"" + height + "\", fixedsize=\"true\"];");

        layout.outgoingArcs(node).foreach({ node2 =>
          out.println("\"" + nodeToId(node) + "\" -> \"" + nodeToId(node2) + "\";");
        })
      })
      out.println("}")
      
      None
    } catch {
      case e => Some(ExportError.Exception(e))
    } finally {
      out.close
    }
  }
}		
