package iteratee

object IterateeContract {

  sealed trait Input[+E] {
    def map[U](f: E => U): Input[U] = this match {
      case El(e) => El(f(e))
      case EOF => EOF
      case Empty => Empty
    }
  }

  case class El[+E](e: E) extends Input[E]

  case object EOF extends Input[Nothing]

  case object Empty extends Input[Nothing]

  sealed trait Step[E, +A]

  object Step {
    case class Done[E, +A](state: A, remaining: Input[E]) extends Step[E, A]
    case class Cont[E, +A](next: Input[E] => Iteratee[E, A]) extends Step[E, A]
  }

  trait Iteratee[E, +A] {
    def fold[B](folder: Step[E, A] => B): B

    def run: A

    def flatMap[B](f: A => Iteratee[E, B]): Iteratee[E, B]

    def map[B](f: A => B): Iteratee[E, B]
  }

  trait Enumerator[E] { parent =>
    def apply[A](e: Iteratee[E, A]): LazyContext[Iteratee[E, A]]

    def through[To](enumerate: Enumeratee[E, To]): Enumerator[To] = new Enumerator[To] {
      def apply[A](e: Iteratee[To, A]): LazyContext[Iteratee[To, A]]= {
        val transformed: Iteratee[E, Iteratee[To, A]] = enumerate.applyOn(e)
        val inner: LazyContext[Iteratee[To, A]] = parent.apply(transformed).map(_.run)
        inner
      }
    }
  }

  trait Enumeratee[From, To] {

    def applyOn[A](i: Iteratee[To, A]): Iteratee[From, Iteratee[To, A]]

    def transform[A](i: Iteratee[To, A]): Iteratee[From, A] 
  }

}