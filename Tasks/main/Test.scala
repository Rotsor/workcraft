package org.workcraft.tasks

// import scalaz.Scalaz
// import scalaz.Scalaz._
import scalaz.effect.IO._
import scalaz.effect.IO
import Task._
import Task.ioMonad
import scalaz.syntax.monad._
import scalaz.Unapply._

object Test {
  def step1 : Task[Unit, Nothing] = Task( tc => {
    System.out.print("Step 1")
    Range(0,30).foreach( x => {
      Thread.sleep(100) 
      System.out.print (".")
      tc.progressUpdate(0.4).unsafePerformIO
    })
    System.out.println
    Right(())
  }.pure[IO])
  
  def step2 : Task[Unit, Nothing] = Task({
    System.out.print("Step 2")
    Range(0,30).foreach( x => {
      Thread.sleep(100) 
      System.out.print (".")
    })
    System.out.println
    Right(())
  }.pure[IO])
  
  def step3 : Task[Unit, Nothing] = Task({
    System.out.print("Step 3")
    Range(0,30).foreach( x => {
      Thread.sleep(100) 
      System.out.print (".")
    })
    System.out.println
    Right(())
  }.pure[IO])
  
  
  def progressUpdate (progress: Double) = {
    System.out.println (progress)    
  }.pure[IO]
  
  def statusUpdate (status: String) = {
    System.out.println ("Now doing: " + status)
  }.pure[IO]
  
  def main(args: Array[String]) : Unit = {
    var cancelled = false
    
    val myComplexTask = (for {
      _ <- ioTask (statusUpdate ("step 1"));
      x <- step1;
      _ <- ioTask (statusUpdate ("step 2"));
      y <- step2;
      _ <- ioTask (statusUpdate ("step 3"))
    } yield ()).flatMap(_ => step3)
    
    myComplexTask.runTask(TaskControl(cancelled.pure, Test.progressUpdate(_), _ => IO {} )).unsafePerformIO
  }
}
