package org.workcraft.plugins.fsm

import scalaz._
import Scalaz._
import java.io.File


object FsmFormatParser {
  class Parser[A](parsef : => (List[Char] => Either[String, (A, List[Char])]),
    printf : A => List[Char] => List[Char]
  ) {
    lazy val parse = parsef
    lazy val print = printf
    lazy val ppp = printf
    def xmap[B](f : A => B, g : B => A) = Parser(
      l => parse(l) match {
        case Left(err) => Left(err)
        case Right((x, rest)) => Right((f(x), rest))
      }, (b : B) => print(g(b))
    )
  }
  object Parser {
    def apply[A](parsef : => (List[Char] => Either[String, (A, List[Char])]),
      printf : A => List[Char] => List[Char]
    ) = new Parser(parsef, printf)
  }
  def seq[A,B](a : => Parser[A], b : => Parser[B]) : Parser[(A, B)] = Parser.apply[(A,B)](
    l => a.parse(l) match {
      case Left(err) => Left(err)
      case Right((a, l)) => b.parse(l) match {
        case Left(err) => Left(err)
        case Right((b, l)) => Right ((a, b), l)
      }
    }
    , {case (av, bv) => s => a.print(av)(b.print(bv)(s))}
  )
  def seql[A,B](a : Parser[A], b : Parser[Unit]) : Parser[A] = seq(a,b).xmap(
    {case (a, ()) => a},
    {case a => (a, ())}
  )
  def seqr[A,B](a : Parser[Unit], b : Parser[B]) : Parser[B] = seq(a,b).xmap(
    {case ((), b) => b},
    {case b => ((), b)})

  def tuple3[A,B,C](a : Parser[A], b : Parser[B], c : Parser[C]) : Parser[(A, B, C)] = seq(seq(a,b), c).xmap(
    { case ((a,b),c) => (a,b,c) },
    { case (a,b,c) => ((a,b),c) }
  )
  def char(ch : Char) : Parser[Unit] = Parser (
    {
      case Nil => Left("expected a char")
      case (c :: cs) => if(c == ch) Right((), cs) else Left("'" + c + "' unexpected!")
    }
    , { case () => (s : List[Char]) => ch :: s }
  )
  def choice[A,B](a1 : => Parser[A], a2 : => Parser[B]) : Parser[Either[A,B]] = Parser(
    l => a1.parse(l) match {
      case Left(_) => a2.parse(l) match {
        case Left(err) => Left(err)
        case Right((b, l)) => Right((Right(b), l))
      }
      case Right((a, l)) => Right((Left(a), l))
    },
    {
      case Left(a) => a1.print(a)
      case Right(b) => a2.print(b)
    }
  )
  def bool : Parser[Boolean]
   = choice(char('-'), char('+')).xmap({
     case Left(()) => false
     case Right(()) => true
   },{
     case false => Left(())
     case true => Right(())
   })
  def option[A] : Parser[A] => Parser[Option[A]]
   = a => choice(char('-'), seq(char('+'), a)).xmap(
     {
       case Left(()) => None
       case Right(((), x)) => Some(x)
     }
       , {
       case None => Left(())
       case Some(x) => Right(((), x))
     }
   )
  // item must be mutually exclusive with end
  def genericList[A](
    item : Parser[A],
    end : Parser[Unit]) = {
    lazy val p : Parser[List[A]] = choice(end, seq(item, p)).xmap({
      case Left(()) => Nil
      case Right((x, l)) => x :: l
    }, {
      case Nil => Left(())
      case x :: l => Right((x, l))
    })
    p
  }

  def stringChar : Parser[Char] = Parser (
    {
      case Nil => Left("string char: Nil")
      case '"' :: _ => Left("string char: \"")
      case '\\' :: c :: cs => Right(c, cs)
      case c :: cs => Right(c, cs)
    },
    {
      case '"' => s => '\\' :: '"' :: s
      case '\\' => s => '\\' :: '\\' :: s
      case c => s => c :: s
    }
  )

  def string : Parser[String] =
    seqr(char('"'), genericList(stringChar, char('"'))).xmap(_.mkString, _.toList)

  // p must not admit a ']' as a first character
  def list[A](p : Parser[A]) : Parser[List[A]] =
      seqr(char('['), genericList(p, char(']')))
  def parser[State, Symbol] : 
      Parser[State] =>
      Parser[Symbol] =>
      Parser[(State, List[(State, List[(Option[Symbol], State)], Boolean)])]
   = state => symbol =>
    seq(state, list(tuple3(state, list(seq(option(symbol), state)), bool)))
  def mainParser = parser(string)(string)

  import org.workcraft.scala.effects.IO.ioPure

  def parseFile(file : File) = ioPure.pure {
    val source = scala.io.Source.fromFile(file.getCanonicalPath)
    val contents = source.mkString.toList
    source.close ()
    mainParser.parse(contents) match {
      case Left(err) => Left(err)
      case Right((res, Nil)) => Right(Nfa.fromData(res))
      case Right((res, _ :: _)) => Left("unused output")
    }
  }

  def writeToFile(file : File, s: String) {
    val pw = new java.io.PrintWriter(file)
    try {
      pw.write(s)
    } finally {
      pw.close()
    }
  }

  def printFile(file : File, d : (String, List[(String, List[(Option[String], String)], Boolean)])) = ioPure.pure {
    writeToFile(file, mainParser.print(d)(Nil).mkString)
  }
}
