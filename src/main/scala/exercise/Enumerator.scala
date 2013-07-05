package exercise

import iteratee.IterateeContract._
import java.io.InputStream
import solution.Iteratee._

object Enumerator {
  def apply[E](e: E*): Enumerator[E] = ???
  
  def fromStream(inputStream: InputStream): Enumerator[Byte] = ???
}
