package exercise

import iteratee.IterateeContract._
import java.io.InputStream

object Enumeratee {
  def map[From] = new {
    def apply[To](f: From => To): Enumeratee[From, To] = ???
  }
}
 
