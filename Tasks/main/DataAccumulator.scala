package org.workcraft.tasks

import scala.collection.mutable.ListBuffer
import scalaz.effect.IO._
import scalaz.effect.IO
import scalaz.Scalaz._
import java.util.Arrays

class DataAccumulator {
  val stderrData = ListBuffer[Array[Byte]]()
  val stdoutData = ListBuffer[Array[Byte]]()
  
  def stdout (data: Array[Byte]) = IO { stdoutData += data } >| {}
  def stderr (data: Array[Byte]) = IO { stderrData += data } >| {}
  
  def collectStdout = IO {
    val length = stdoutData.foldLeft(0)(_+_.length)
    val result = new Array[Byte](length)
    stdoutData.foldLeft(0)( (offset, chunk) => { System.arraycopy(chunk, 0, result, offset, chunk.length); offset+chunk.length} )
    result
  }
  
  def collectStderr = IO {
    val length = stderrData.foldLeft(0)(_+_.length)
    val result = new Array[Byte](length)
    stderrData.foldLeft(0)( (offset, chunk) => { System.arraycopy(chunk, 0, result, offset, chunk.length); offset+chunk.length} )
    result
  }  
}
