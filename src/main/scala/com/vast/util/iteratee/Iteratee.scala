package com.vast.util.iteratee

import scala.util.{Success, Failure, Try}

/**
 * An Iteratee implementation based on the version provided by the Play framework. This implementation is quite similar to
 * theirs, except that the Iteratee itself abandons the concept of having the ability to asynchrounously compute
 * its next state. Removing this does limit the range of calculations that can be made on a stream,
 * but removing Futures from the pipeline gives a significant performance improvement. This version of Iteratee is
 * suitable to use if you do not require async behavior on a step - namely, the feed operation returns an Iteratee directly, not
 * a Future[Iteratee]. If you require this behavior, consider using the Play version instead.
 *
 * @author dpratt@vast.com
 */
trait Iteratee[E, +A] {
  self =>

  def fold[B](folder: Step[E, A] => B): B

  def feed[AA >: A](in: Input[E]): Iteratee[E, AA] = {
    self.fold {
      case Step.Done(value, remaining) => Done(value, remaining)
      case Step.Cont(k) => k(in)
      case Step.Error(msg, remaining) => Error(msg, remaining)
    }
  }

  def map[B](f: A => B): Iteratee[E, B] = this.flatMap(a => Done(f(a), Input.Empty))

  def flatMap[B](f: A => Iteratee[E, B]): Iteratee[E, B] = {
    self.fold {
      case Step.Done(a, Input.Empty) => f(a)
      case Step.Done(a, e) => f(a).fold {
        case Step.Done(value, _) => Done(value, e)
        case Step.Cont(k) => k(e)
        case Step.Error(msg, remaining) => Error(msg, remaining)
      }
      case Step.Cont(k) => Cont(in => k(in).flatMap(f))
      case Step.Error(msg, e) => Error(msg, e)
    }
  }

  def isDoneOrError: Boolean = fold {
    case Step.Cont(_) => false
    case _ => true
  }

  def run: Try[A] = {
    self.fold {
      case Step.Cont(k) =>
        //if it's not done, send an EOF and hope that it finishes after that
        k(Input.EOF).fold {
          case Step.Cont(_) => Failure(new IterateeException("Expected valuesHandler Iteratee to complete after EOF."))
          case Step.Done(x, _) => Success(x)
          case Step.Error(t, _) => Failure(t)
        }
      case Step.Done(x, _) => Success(x)
      case Step.Error(t, _) => Failure(t)
    }
  }

  def flatFold[B, C](folder: Step[E, A] => Iteratee[B, C]): Iteratee[B, C] = fold(folder)

  def joinI[AIn](implicit in: A <:< Iteratee[_, AIn]): Iteratee[E, AIn] = {
    this.flatMap { a =>
      val inner = in(a)
      inner.flatFold[E, AIn] {
        case Step.Done(value, _) => Done(value, Input.Empty)
        case Step.Cont(nextState) => nextState(Input.EOF).flatFold[E, AIn] {
          case Step.Done(innerValue, _) => Done(innerValue, Input.Empty)
          case Step.Cont(_) => Error(new IterateeException("divergent inner iteratee on joinI after EOF"), Input.EOF)
          case Step.Error(t, _) => Error(t, Input.Empty)
        }
        case Step.Error(t, _) => Error(t, Input.Empty)
      }
    }
  }

}

/**
 * Returned from an Error state when an internal problem prevents an Interatee from completing. Iteratees
 * can return any Throwable, and clients should not use IterateeException unless something dealing with the
 * actual internal implementation of the Iteratee has gone wrong. For other cases, use a more meaningful
 * exception.
 */
class IterateeException(msg: String, cause: Throwable) extends RuntimeException(msg, cause) {
  def this(msg: String) {
    this(msg, null)
  }
}

object Iteratee {

  /**
   * Create an [[com.vast.util.iteratee.Iteratee]] which folds the content of the Input using a given function and an initial state
   *
   * Example:
   * {{{
   *   // Count the number of input elements
   *   def count[E]: Iteratee[E, Int] = Iteratee.fold(0)((c, _) => c + 1)
   * }}}
   *
   * @param state initial state
   * @param f a function folding the previous state and an input to a new state
   */
  def fold[E, A](state: => A)(f: (A, E) => A): Iteratee[E, A] = {
    def step(s: A)(i: Input[E]): Iteratee[E, A] = i match {
      case Input.EOF => Done(s, Input.EOF)
      case Input.Empty => Cont[E, A](step(s))
      case Input.El(e) => Cont(step(f(s, e)))
    }
    Cont(step(state))
  }

  /**
   * Create an iteratee that takes the first element of the stream, if one occurs before EOF
   */
  def head[E]: Iteratee[E, Option[E]] = {
    def step: Input[E] => Iteratee[E, Option[E]] = {
      case Input.Empty => Cont(step)
      case Input.EOF => Done(None, Input.EOF)
      case Input.El(e) => Done(Some(e), Input.Empty)
    }
    Cont(step)
  }

  /**
   * Consume all the chunks from the stream, and return a list.
   */
  def getChunks[E]: Iteratee[E, List[E]] = fold[E, List[E]](Nil) { (els, chunk) =>
    chunk :: els
  }.map(_.reverse)

  def foreach[E](f: E => Unit): Iteratee[E, Unit] = fold[E, Unit](())((_, e) => f(e))
}

/**
 * Defines the three states that any Iteratee can be in.
 */
sealed trait Step[E, +A] {
  lazy val it: Iteratee[E, A] = this match {
    case Step.Done(a, e) => Done(a, e)
    case Step.Cont(k) => Cont(k)
    case Step.Error(msg, e) => Error(msg, e)
  }
}

object Step {
  case class Done[+A, E](a: A, remaining: Input[E]) extends Step[E, A]
  case class Cont[E, +A](k: Input[E] => Iteratee[E, A]) extends Step[E, A]
  case class Error[E](msg: Throwable, input: Input[E]) extends Step[E, Nothing]
}

/**
 * Represents a single chunk of input to an [[com.vast.util.iteratee.Iteratee]]. An input can be one of three types -
 *  i. A valid chunk, represented by [[com.vast.util.iteratee.Input.El]]
 *  i. An empty chunk, represented by [[com.vast.util.iteratee.Input.Empty]]
 *  i. EOF, represented by [[com.vast.util.iteratee.Input.EOF]]
 */
sealed trait Input[+E] {
  def map[U](f: (E => U)): Input[U] = this match {
    case Input.El(e) => Input.El(f(e))
    case Input.Empty => Input.Empty
    case Input.EOF => Input.EOF
  }
}

object Input {
  case class El[+E](e: E) extends Input[E]
  case object Empty extends Input[Nothing]
  case object EOF extends Input[Nothing]
}

//Handy objects used to create Iteratees in various states.
object Done {
  def apply[E, A](a: A, e: Input[E] = Input.Empty): Iteratee[E, A] = new Iteratee[E, A] {
    def fold[B](folder: Step[E, A] => B): B = folder(Step.Done(a, e))
  }
}

object Cont {
  def apply[E, A](k: Input[E] => Iteratee[E, A]): Iteratee[E, A] = new Iteratee[E, A] {
    def fold[B](folder: Step[E, A] => B): B = folder(Step.Cont(k))
  }
}

object Error {
  def apply[E](msg: Throwable, e: Input[E]): Iteratee[E, Nothing] = new Iteratee[E, Nothing] {
    def fold[B](folder: Step[E, Nothing] => B): B = folder(Step.Error(msg, e))
  }
}
