package com.vast.util.iteratee

/**
 * A set of low-level [[com.vast.util.iteratee.Iteratee]]s that assist with parsing a stream of generic values.
 *
 * @author David Pratt (dpratt@vast.com)
 */
object Combinators {

  def dropWhile[A](f: A => Boolean): Iteratee[A, Unit] = Cont {
    case in@Input.El(event) => if( f(event) ) dropWhile(f) else Done(Unit, in)
    case in@Input.EOF => Done(Unit, in)
    case _ => dropWhile(f)
  }

  def takeWhile[A](f: A => Boolean): Iteratee[A, Seq[A]] = {
    def step(acc: Seq[A]): Iteratee[A, Seq[A]] = Cont {
      case in@Input.El(event) => if(f(event)) step(acc :+ event) else Done(acc, in)
      case in@Input.EOF => Done(acc, in)
      case _ => step(acc)
    }

    step(Seq[A]())
  }

  def takeOne[A]: Iteratee[A, A] = Cont {
    case Input.El(event) => Done(event)
    case Input.EOF => Error(new IterateeException("Unexpected EOF in takeOne"), Input.Empty)
    case Input.Empty => takeOne
  }

  def peekOne[A]: Iteratee[A, A] = Cont {
    case in@Input.El(event) => Done(event, in)
    case Input.Empty => peekOne
    case Input.EOF => Error(new IterateeException("Unexpected EOF in peekOne"), Input.Empty)
  }

  def drop[A](n: Int): Iteratee[A, Unit] =
    Cont {
      case in@Input.EOF => Done(Unit, in)
      case Input.Empty => drop(n)
      case in@Input.El(data) => {
        if (n <= 0) {
          Done(Unit, in)
        } else {
          drop(n - 1)
        }
      }
    }

  /**
   * An interatee that consumes the current token iff it satisfies the supplied scanner. The result
   * of the iteratee will be the value resulting from the scanner.
   */
  def expect[A](f: A => Boolean): Iteratee[A, A] = takeOne.flatMap { event =>
    if(!f(event)) {
      Error(new IterateeException("Missing expected value."), Input.Empty)
    } else {
      Done(event)
    }
  }

}
