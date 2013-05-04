package org.workcraft.logging
import java.text.SimpleDateFormat
import java.util.Date
import scalaz.effect.IO
import scalaz.effect.IO._
import scalaz.Scalaz._

class StandardStreamLogger extends Logger[IO] {
  val DEBUG   = "   DEBUG| "
  val INFO    = "    INFO| "
  val WARNING = " WARNING| "
  val ERROR   = "   ERROR| "
  val DUMMY   = "                           | " 
  
  val dateFormatString = "dd.MM.yyyy HH:mm:ss"
  val dateFormat = new SimpleDateFormat(dateFormatString)
  
  private def format (message: String, prefix: String) = 
    dateFormat.format(new Date()) + prefix + message
    
  private def print (stream: java.io.PrintStream, message:String, prefix: String) = {
    val lines = message.split("\n").toList
    stream.println (format(lines.head, prefix))
    lines.tail.foreach( s => stream.println (DUMMY + s))
  }
  
  def log (message: String, klass: MessageClass) = klass match {
    case MessageClass.Debug => print (System.out, message, DEBUG).pure[IO]
    case MessageClass.Info => print (System.out, message, INFO).pure[IO]
    case MessageClass.Warning => print (System.err, message, WARNING).pure[IO]
    case MessageClass.Error => print (System.err, message, ERROR).pure[IO]
  } 
}
