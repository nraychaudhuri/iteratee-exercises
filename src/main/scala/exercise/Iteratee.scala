package exercise

import iteratee.IterateeContract._
import java.io.InputStream
object Iteratee {

  object Enumerator {
    def apply[E](e: E*): Enumerator[E] = ???
    
    def fromStream(inputStream: InputStream, chunkSize: Int): Enumerator[Array[Byte]] = ???
  }
  
  
  object Enumeratee {
    def map[From] = new {
      def apply[To](f: From => To): Enumeratee[From, To] = ???
    }
    
    def takeWhile[E](f: E => Boolean): Enumeratee[E, E] = ???
    
    def take[E](count: Int): Enumeratee[E, E] = ???
  }
 
  def counter[E]: Iteratee[E, Int] = ???

  def sum[Int]: Iteratee[Int, Int] = ???

  def head[E]: Iteratee[E, Option[E]] = ???
  
  def dropN[E](count: Int): Iteratee[E, Unit] = ???
  
  
  def collect[E]: Iteratee[E, List[E]] = ???
  
  def ignore[E]: Iteratee[E, Unit] = ???
  
  
  def lineIteratee: Iteratee[Char, String] = for {
    chars <- Enumeratee.takeWhile[Char](_ != '\n').transform(collect)
    _ <- Enumeratee.take(1).transform(ignore[Char])
  } yield chars.mkString
  
  def getElementAt[E](count: Int): Iteratee[E, Option[E]] = for {
    _ <- dropN(count)
    e <- head
  } yield e

}