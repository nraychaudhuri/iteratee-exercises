package exercise

import org.specs2.mutable._

import Iteratee._

class ExerciseSpec extends Specification {

  "Warming up" should {
    "count number of elements in list " in {
      Enumerator(1, 2, 3, 4).apply(counter).run must_== 4	
    }
    "sum all the numbers" in {
      Enumerator(1, 2, 3, 4).apply(sum).run must_== 10	
    }
    "find the head element'" in {
      Enumerator("a", "b", "c", "d").apply(head).run must_== "a"
    }
  }
  
  class MyStream(bytes: Array[Byte]) extends java.io.ByteArrayInputStream(bytes) {
    private[this] var _isClosed = false
    def isClosed = _isClosed
    override def close(): Unit = {
      _isClosed = true
      super.close()
    }
  }
  
  "Reading from file" should {
    "counter bytes from file" in {
      
      val stream = new MyStream("1234567".getBytes())      
      Enumerator.fromStream(stream, chunkSize = 1).apply(counter).run must_== 7      
      stream.isClosed must beTrue
    }
    
    "getting just the head element" in {
      val stream = new MyStream("1234".getBytes())      
      Enumerator.fromStream(stream, chunkSize = 1).apply(head).run must_== 1      
      stream.isClosed must beTrue
      
    }

    "getting just the 4th element" in {
      val stream = new MyStream("abcdef".getBytes()) 
      Enumerator.fromStream(stream, chunkSize = 1).apply(getElementAt(3)).run must_== "c"  
      stream.isClosed must beTrue      
    }    
  }
  
  
  "Doing transformation" should {
    "ints to strings" in {
      val chars = 'a' to 'z' 
      Enumerator(chars:_*).
      	through(Enumeratee.map(i => i.toString.toUpperCase)).
      	apply(collect).run must_== ('A' to 'Z' toList)
    }
    
    "read first line" in {
      Enumerator(List('a', 'b', 'c', '\n', '1', '2', '3'):_*).apply(lineIteratee).run must_== "abc"
    }
  }
}