


import solution.Iteratee
import iteratee.IterateeContract._
import iteratee.LazyContext

object Experiment extends App {
  import Iteratee._
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

  def dropN[E](count: Int): Iteratee[E, Unit] = {
    def step(counter: Int): Input[E] => Iteratee[E, Unit] = {
      case El(e) if counter < count => Cont(step(counter + 1))
      case El(e) => Done((), El(e))
      case EOF => Done((), EOF)
      case Empty => Cont(step(counter))
    }
    Cont(step(0))
  } 

  def sum: Iteratee[Int, Int] = {
    def step(counter: Int): Input[Int] => Iteratee[Int, Int] = {
      case El(e)  => 
        Cont(step(counter + e))
      case EOF => 
        Done(counter, EOF)
      case Empty => 
        Cont(step(counter))
    }
    Cont(step(0))
  }  

  def getElementAt[E](count: Int): Iteratee[E, Option[E]] = for {
    _ <- dropN(count)
    e <- head
  } yield e

  class MyStream(bytes: Array[Byte]) extends java.io.ByteArrayInputStream(bytes) {
    override def close(): Unit = {
      println("Close is invoked")
      super.close()
    }
  }
  val input = new MyStream("1234".getBytes())

  val arrayToInt: Enumeratee[Array[Byte], Int] = 
    Enumeratee.map[Array[Byte]](bytes => {
      new String(bytes).toInt
    })

    
  

  println(">>>>>> " + Enumerator.fromStream(input, 1).through(arrayToInt).apply(sum).run.run)
  // println("Number of bytes in the string >>> " + 
  //   new String(Enumerator.fromStream(input, 1).apply(getElementAt(6)).run.get)
  // )




}