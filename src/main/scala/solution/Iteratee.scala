
package solution

import iteratee.IterateeContract._
import iteratee.LazyContext

object Iteratee {

  trait SolutionIteratee[E, +A] extends Iteratee[E, A]{

    def run: A = fold {
      case Step.Done(a, _) => a
      case Step.Cont(k) => k(EOF).fold {
        case Step.Done(a, _) => a
        case Step.Cont(_) => sys.error("diverging iteratee after Input.EOF")
      }
    }

    def flatMap[B](f: A => Iteratee[E, B]): Iteratee[E, B] = this match {
      case Done(a, e) => f(a) match {
        case Done(a, _) => Done(a, e)
        case Cont(k) => k(e)
      }
      case Cont(k) => Cont(e => k(e) flatMap f)
    }

    def map[B](f: A => B): Iteratee[E, B] = this match {
      case Done(a, e) => Done(f(a), e)
      case Cont(k) => Cont(e => k(e) map f)
    }
  }

  case class Done[E, +A](state: A, remaining: Input[E]) extends SolutionIteratee[E, A] {
    def fold[B](folder: Step[E, A] => B): B = folder(Step.Done(state, remaining))
  }

  case class Cont[E, +A](k: Input[E] => Iteratee[E, A]) extends SolutionIteratee[E, A] {
    def fold[B](folder: Step[E, A] => B): B = folder(Step.Cont(k))    
  }
}
