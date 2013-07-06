package exercise

import iteratee.IterateeContract._
import java.io.InputStream

object Exercise {

  def counter[E]: Iteratee[E, Int] = ???

  def sum: Iteratee[Int, Int] = ???

  def head[E]: Iteratee[E, Option[E]] = ???
  
  def dropN[E](count: Int): Iteratee[E, Unit] = ???
  
  
  def collect[E]: Iteratee[E, List[E]] = ???
  
  def ignore[E]: Iteratee[E, Unit] = ???
  
  def takeWhile[E](f: E => Boolean): Enumeratee[E, E] = ???
  
  def take[E](count: Int): Enumeratee[E, E] = ???

  def lineIteratee: Iteratee[Char, String] = for {
    chars <- takeWhile[Char](_ != '\n').transform(collect)
    _ <- take(1).transform(ignore[Char])
  } yield chars.mkString
  
  def getElementAt[E](count: Int): Iteratee[E, Option[E]] = for {
    _ <- dropN(count)
    e <- head
  } yield e

}
