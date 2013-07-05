package exercise

import iteratee.IterateeContract._
import java.io.InputStream
object Iteratee {
  
  trait SolutionIteratee[E, +A] extends Iteratee[E, A]{

    def run: A = ???

    def flatMap[B](f: A => Iteratee[E, B]): Iteratee[E, B] = ???

    def map[B](f: A => B): Iteratee[E, B] = ???
  }
  
  case class Done[E, +A](state: A, remaining: Input[E]) extends SolutionIteratee[E, A] {
    def fold[B](folder: Step[E, A] => B): B = folder(Step.Done(state, remaining))
  }

  case class Cont[E, +A](k: Input[E] => Iteratee[E, A]) extends SolutionIteratee[E, A] {
    def fold[B](folder: Step[E, A] => B): B = folder(Step.Cont(k))    
  }
}