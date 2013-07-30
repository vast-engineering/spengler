package com.vast.util.iteratee

/**
 * An Enumeratee is a class that can transform a stream of inputs from type From to type To. This can be used to both
 * transform an Iteratee[From, A] to Iteratee[To, A] and also Enumerator[From] to Enumerator[To].
 *
 * @author dpratt@vast.com
 */
trait Enumeratee[From, To] {
  parent =>

  /**
   * Create a new Iteratee that feeds its input, potentially modifying it along the way, into the inner Iteratee, and
   * produces that Iteratee as its result. This is the main method that all Enumeratee subclasses must implement.
   */
  def applyOn[A](inner: Iteratee[To, A]): Iteratee[From, Iteratee[To, A]]

  /**
   * Transform an Iteratee[From, A] to Iteratee[To, A]
   */
  def transform[A](inner: Iteratee[To, A]): Iteratee[From, A] = applyOn(inner).joinI

  /**
   * Compose this Enumeratee with another Enumeratee
   */
  def compose[To2](other: Enumeratee[To, To2]): Enumeratee[From, To2] = {
    new Enumeratee[From, To2] {
      def applyOn[A](iteratee: Iteratee[To2, A]): Iteratee[From, Iteratee[To2, A]] = {
        parent.applyOn(other.applyOn(iteratee)).joinI
      }
    }
  }
}

object Enumeratee {

  /**
   * Create an Enumeratee that transforms its input using the given function.
   *
   * This is like the `map` function, except that it allows the Enumeratee to, for example, send EOF to the inner
   * iteratee before EOF is encountered.
   * @param f Used to transform each input element.
   */
  def mapInput[From, To](f: Input[From] => Input[To]) = new Enumeratee[From, To] {
    def applyOn[A](inner: Iteratee[To, A]): Iteratee[From, Iteratee[To, A]] = {
      def step(it: Iteratee[To, A])(in: Input[From]): Iteratee[From, Iteratee[To, A]] = {
        it.fold {
          case Step.Done(value, remaining) => Done(it)
          case Step.Error(t, remaining) => Error(t, Input.Empty)
          case Step.Cont(k) => Cont(step(k(f(in))))
        }
      }
      Cont(step(inner))
    }
  }

  /**
   * Create an Enumeratee which transforms its input using a given function

   * @param f A function to transform input elements.
   */
  def map[From, To](f: From => To) = mapInput((in: Input[From]) => in.map(f))

}

