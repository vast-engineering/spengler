package com.vast.xml

import play.api.libs.iteratee._
import scala.concurrent.ExecutionContext

/**
 *
 * @author David Pratt (dpratt@vast.com)
 */
object Combinators {

  def dropWhile(f: XMLEvent => Boolean): Iteratee[XMLEvent, Unit] = Cont {
    case in@Input.El(event) => if( f(event) ) dropWhile(f) else Done(Unit, in)
    case in@Input.EOF => Done(Unit, in)
    case _ => dropWhile(f)
  }

  def takeWhile(f: XMLEvent => Boolean): Iteratee[XMLEvent, Seq[XMLEvent]] = {
    def step(acc: Seq[XMLEvent]): Iteratee[XMLEvent, Seq[XMLEvent]] = Cont {
      case in@Input.El(event) => if(f(event)) step(acc :+ event) else Done(acc, in)
      case in@Input.EOF => Done(acc, in)
      case _ => step(acc)
    }

    step(Seq[XMLEvent]())
  }

  def takeOne: Iteratee[XMLEvent, XMLEvent] = Cont {
    case in@Input.El(event) => Done(event)
    case in@Input.EOF => Error("Unexpected EOF.", in)
    case Input.Empty => takeOne
  }

  def peekOne: Iteratee[XMLEvent, XMLEvent] = Cont {
    case in@Input.El(event) => Done(event, in)
    case Input.Empty => peekOne
    case in@Input.EOF => Error("Unexpected EOF.", in)
  }

  def drop(n: Int): Iteratee[XMLEvent, Unit] =
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


  type ScanResult[A] = Either[String, A]
  type Scanner[A, B] = A => ScanResult[B]

  def scanner[A](f: XMLEvent => ScanResult[A]): Scanner[XMLEvent, A] = {
    f
  }

  /**
   * An interatee that consumes the current token iff it satisfies the supplied scanner. The result
   * of the iteratee will be the value resulting from the scanner.
   */
  def expect[A](f: Scanner[XMLEvent, A])(implicit ec: ExecutionContext): Iteratee[XMLEvent, A] = takeOne.flatMap { event =>
    f(event).fold(
      message => Error(message, Input.Empty),
      x => Done(x)
    )
  }

}
