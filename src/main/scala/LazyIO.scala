

import java.io.BufferedReader
import java.io.FileReader

class LazyIO {

  def getContents(fileName: String): Iterable[String] = {
    val fr = new BufferedReader(new FileReader(fileName))
    new Iterable[String] {
      def iterator = new Iterator[String] {
        def hasNext = line != null
        def next = {
          val retVal = line; line = getLine; 
          retVal
        }
        def getLine = { fr.readLine }
        var line = getLine
      }
    }
  }

}