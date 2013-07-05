package exercise

import iteratee.IterateeContract._
import java.io.InputStream
import iteratee.LazyContext

object Enumerator {
  def apply[E](e: E*): Enumerator[E] = ???
  
  def fromStream(inputStream: InputStream): Enumerator[Byte] = ???
}
