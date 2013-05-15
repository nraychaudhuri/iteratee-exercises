
import java.io._
import solution.Iteratee

object WrappingInMonad extends App {
  
  import iteratee.IterateeContract._
  import iteratee.LazyContext
  import solution.Iteratee._
  
  def counter[E]: Iteratee[E, Int] = {
    def step(count: Int): Input[E] => Iteratee[E, Int] = {
      case El(e) => Cont(step(count + 1))
      case EOF => Done(count, EOF)
      case Empty => Cont(step(count))
    }
    Cont(step(0))
  }
  
  def head[E]: Iteratee[E, Option[E]] = {
    def step: Input[E] => Iteratee[E, Option[E]] = {
      case El(e) => Done(Option(e), Empty)
      case EOF => Done(None, EOF)
      case Empty => Cont(step)
    }
    Cont(step)
  }
  
  
  class MyStream(bytes: Array[Byte]) extends java.io.ByteArrayInputStream(bytes) {
    override def close(): Unit = {
      println("Close is invoked")
      super.close()
    }
  }

  val lineIteratee: Iteratee[Char, String] = for {
    chars <- Enumeratee.takeWhile[Char](_ != '\n').transform(getChunks)
    _ <- Enumeratee.take(1).transform(ignore[Char]) 
  } yield chars.mkString
  
  def enumerate[E, A](xs: List[E])(i: Iteratee[E, A]): Iteratee[E, A] = (xs, i) match {
    case (Nil, i) => i
    case (_, i@Done(_, _)) => i
    case (x :: xs, Cont(k)) => enumerate(xs)(k(El(x))) 
  }
  
  val asLines: Enumeratee[Char, String] = Enumeratee.grouped(lineIteratee)
  println(">>>> " + Enumerator(List('a', 'b', 'c', '\n', '1', '2', '3'):_*).through(asLines).apply(counter).run.run)
  
}







