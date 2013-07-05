package solution

import iteratee.IterateeContract._
import iteratee.LazyContext
import solution.Iteratee._

object Enumerator {
  def apply[E](e: E*) = new Enumerator[E] {
    def apply[A](i: Iteratee[E, A]): LazyContext[Iteratee[E, A]] = {
      def step(xs: Seq[E])(i: Iteratee[E, A]): Iteratee[E, A] = (xs, i) match {
        case (Nil, Cont(k)) => k(EOF)
        case (x +: xs, Cont(k)) => step(xs)(k(El(x)))
        case (_, it@Done(_, _)) => i
      }        
      LazyContext(step(e)(i))
    }
  }
  
  import java.io._
  def fromStream(inputStream: InputStream): Enumerator[Byte] = new Enumerator[Byte]{
    def readChunk = inputStream.read match {
      case -1 => None
      case byte => Some(byte.toByte)
    }  
    def apply[A](i: Iteratee[Byte, A]): LazyContext[Iteratee[Byte, A]] = {(readChunk, i) match {
      case (None, Cont(k)) => LazyContext(k(EOF))
      case (Some(bytes), Cont(k)) => 
        apply(k(El(bytes)))
      case (_, i@Done(_, _)) => LazyContext(i)
    } } map (i => {inputStream.close(); i})
  }

}
