package solution

import iteratee.IterateeContract._
import java.io.InputStream
import solution.Iteratee._

object Exercise {

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
  
  def collect[E]: Iteratee[E, List[E]] = {
    def step(xs: List[E]): Input[E] => Iteratee[E, List[E]] = {
      case El(e) => Cont(step(xs :+ e))
      case Empty => Cont(step(xs))
      case EOF => Done(xs, EOF)
    }
    Cont(step(Nil))
  }
  
  def ignore[E]: Iteratee[E, Unit] = {
    Cont {
      case El(e) => Done((), Empty)
      case Empty => ignore
      case EOF => Done((), EOF)
    }
  }
  
  def takeWhile[E](f: E => Boolean): Enumeratee[E, E] = new SolutionEnumeratee[E, E] {
    def step[A](k: Input[E] => Iteratee[E, A]): Input[E] => Iteratee[E, Iteratee[E, A]] = {
      case in@El(e) if f(e) => 
        new SolutionEnumeratee[E, E] {
          def continue[A](k: Input[E] => Iteratee[E, A]): Iteratee[E, Iteratee[E, A]] = Cont(step(k))   
        }.applyOn(k(in))
      case in@Empty => 
        new SolutionEnumeratee[E, E] {
          def continue[A](k: Input[E] => Iteratee[E, A]): Iteratee[E, Iteratee[E, A]] = Cont(step(k))   
        }.applyOn(k(in))  
      case _ => 
        Done(Cont(k), EOF)
    }
    def continue[A](k: Input[E] => Iteratee[E, A]): Iteratee[E, Iteratee[E, A]] = Cont(step(k))
  }
  
  def take[E](count: Int): Enumeratee[E, E] = new SolutionEnumeratee[E, E] {
    def step[A](remaining: Int)(k: Input[E] => Iteratee[E, A]): Input[E] => Iteratee[E, Iteratee[E, A]] = {
      case in@El(_) if remaining == 1 => 
        Done(k(in), Empty)
      case in@(El(_) | Empty) if remaining > 1 => 
        new SolutionEnumeratee[E, E] {
          def continue[A](k: Input[E] => Iteratee[E, A]): Iteratee[E, Iteratee[E, A]] = Cont(step(remaining - 1)(k))   
        }.applyOn(k(in))  
      case EOF => 
        Done(Cont(k), EOF)
        
      case in => Done(Cont(k), in)  
    }
    def continue[A](k: Input[E] => Iteratee[E, A]): Iteratee[E, Iteratee[E, A]] = Cont(step(count)(k))      
  }

  def lineIteratee: Iteratee[Char, String] = for {
    chars <- takeWhile[Char](_ != '\n').transform(collect)
    _ <- take(1).transform(ignore[Char])
  } yield chars.mkString
  
  def getElementAt[E](count: Int): Iteratee[E, Option[E]] = for {
    _ <- dropN(count)
    e <- head
  } yield e

}