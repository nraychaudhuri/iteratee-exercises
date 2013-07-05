package solution

import iteratee.IterateeContract._
import iteratee.LazyContext
import solution.Iteratee._

object Enumeratee {
  def map[From] = new {
    def apply[To](f: From => To): Enumeratee[From, To] = new SolutionEnumeratee[From, To] {

      def step[A](k: Input[To] => Iteratee[To, A]): Input[From] => Iteratee[From, Iteratee[To, A]] = {
        case in@(El(_) | Empty) => 
          val input: Input[To] = in.map(f)
          val inner = k(input)
          new SolutionEnumeratee[From, To] {
            def continue[A](k: Input[To] => Iteratee[To, A]): Iteratee[From, Iteratee[To, A]] = Cont(step(k))   
          }.applyOn(inner)
        case _ => 
          Done(Cont(k), EOF)
      }
      def continue[A](k: Input[To] => Iteratee[To, A]): Iteratee[From, Iteratee[To, A]] = Cont(step(k))
    }
  }
  
  def grouped[From] = new {
    def apply[To](folder: Iteratee[From, To]): Enumeratee[From, To] = new SolutionEnumeratee[From, To] {
      def step[A](folder: Iteratee[From, To])(k: Input[To] => Iteratee[To, A]): Input[From] => Iteratee[From, Iteratee[To, A]] = {
        case in@(El(_) | Empty) => 
          val i = new Enumerator[From] {
            def apply[A](i: Iteratee[From, A]): LazyContext[Iteratee[From, A]] = {
              LazyContext {
                i.fold {
                  case Step.Cont(k) => k(in)
                  case _ => i
                }
              }
            }
          }
          i.apply(folder).run.fold {
            case Step.Cont(kF) => Cont(step(Cont(kF))(k))
            case Step.Done(a, left) => new SolutionEnumeratee[From, To] {
              def continue[A](k: Input[To] => Iteratee[To, A]): Iteratee[From, Iteratee[To, A]] = left match {
                case in@El(_) => step(folder)(k)(left)
                case _ => Cont(step(folder)(k))    
              }
            }.applyOn(k(El(a)))
          }
          
        case EOF => 
        
          folder.map {
            case to => Done(k(El(to)), EOF).run 
          }
      }
      def continue[A](k: Input[To] => Iteratee[To, A]): Iteratee[From, Iteratee[To, A]] = Cont(step(folder)(k))        
    }
  }  
}  

trait SolutionEnumeratee[From, To] extends Enumeratee[From, To] {

  def continue[A](k: Input[To] => Iteratee[To, A]): Iteratee[From, Iteratee[To, A]] 

  def applyOn[A](i: Iteratee[To, A]): Iteratee[From, Iteratee[To, A]] = {
    i.fold {
      case Step.Cont(k) => continue(k)
      case _ => Done(i, Empty)
    }
  } 
  
  def transform[A](i: Iteratee[To, A]): Iteratee[From, A] = applyOn(i).flatMap { inner =>
    inner.fold {
      case Step.Done(a, _) => Done(a, Empty)
      case Step.Cont(k) => k(EOF).fold {
        case Step.Done(a, _) => Done(a, Empty)
        case Step.Cont(_) => sys.error("diverging iteratee after Input.EOF")          
      }
    }
  }
}

