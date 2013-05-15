package iteratee

trait LazyContext[A] {

  def run: A
  
  def map[B](f: A => B): LazyContext[B] = LazyContext(f(run))
  
  def flatMap[B](f: A => LazyContext[B]): LazyContext[B] = LazyContext(f(run).run)
}


object LazyContext {
  
  def apply[A](a: => A): LazyContext[A] = new LazyContext[A] {
    def run = a
  }
}