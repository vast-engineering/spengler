package com.vast.util.iteratee

import scala.concurrent.{Promise, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal
import com.typesafe.scalalogging.slf4j.Logging

/**
 * An Enumerator will produce a stream of inputs of type E, and feed each one in turn to a given Iteratee. Notably,
 * the apply() method here allows for asynchronous execution. This is to allow an Enumerator to produce an async stream
 * of inputs - for example, from a network socket.
 *
 * @author dpratt@vast.com
 */
trait Enumerator[E] {
  parent =>

  /**
   * Use this Enumerator to feed input to an Iteratee. The feeding of the input may execute asynchronously.
   *
   */
  def apply[A](i: Iteratee[E, A]): Future[Iteratee[E, A]]

  /**
   * Apply this Enumerator to an Iteratee, and when done, feed EOF and return the resulting value. If the Iteratee
   * has not completed or is in the error state after the EOF is fed, the resulting Future will be completed with an error.
   */
  def run[A](i: Iteratee[E, A])(implicit ec: ExecutionContext): Future[A] = apply(i).flatMap { it =>
    Promise[A]().complete(it.run).future
  }

  /**
   * Compose this Enumerator with an Enumeratee. This operation essentially transforms the input type of a given
   * enumerator from type E to another type To.
   */
  def through[To](enumeratee: Enumeratee[E, To])(implicit ec: ExecutionContext): Enumerator[To] = new Enumerator[To] {
    def apply[A](i: Iteratee[To, A]): Future[Iteratee[To, A]] = {
      val transformed = enumeratee.applyOn(i)
      parent.run(transformed)
    }
  }

  /**
   * Produce a new Enumerator which will feed the inputs from this Enumerator, and when finished supply the inputs
   * from another Enumerator.
   */
  def andThen(e: Enumerator[E])(implicit ec: ExecutionContext): Enumerator[E] = new Enumerator[E] {
    def apply[A](i: Iteratee[E, A]): Future[Iteratee[E, A]] = parent.apply(i).flatMap(e.apply)
  }


}

object Enumerator extends Logging {

  /**
   * Creates an enumerator which produces the one supplied
   * input and nothing else. This enumerator will NOT
   * automatically produce Input.EOF after the given input.
   */
  def enumInput[E](e: Input[E]) = new Enumerator[E] {
    def apply[A](i: Iteratee[E, A]): Future[Iteratee[E, A]] =
      Future.successful(i.feed(e))
  }

  /**
   * Enumerate an iterable.
   */
  def enumIterable[E](e: Iterable[E]) = new Enumerator[E] {
    def apply[A](i: Iteratee[E, A]): Future[Iteratee[E, A]] = {
      try {
        var it = i
        val iterator = e.iterator
        var done = false
        while(iterator.hasNext && !done) {
          it = it.feed(Input.El(iterator.next()))
          done = it.isDoneOrError
        }
        Future.successful(it)
      } catch {
        case NonFatal(t) => Future.failed(t)
      }
    }
  }

  /**
   * An enumerator that feeds EOF and nothing else.
   */
  def eof[A] = enumInput[A](Input.EOF)

  /**
   * An Enumerator that allows calculation of the next input at each state. This enumerator takes an initial state,
   * and then invokes the supplied input function. If the result is a Some, it will be fed to the Iteratee. If it's a None,
   * the Enumerator will stop. Note - this Enumerator will *NOT* feed EOF when the input function signals the end of input.
   *
   */
  def unfold[S, E](s: S)(f: S => Option[(S, E)]): Enumerator[E] = new Enumerator[E] {
    def apply[A](i: Iteratee[E, A]): Future[Iteratee[E, A]] = {
      try {
        //could make this tailrec, but iteration is easier, easier to understand, and slightly faster
        var done = false
        var state = s
        var it = i
        while(!done) {
          val resultOpt = f(state)
          if(resultOpt.isDefined) {
            val (newState, input) = resultOpt.get
            it = it.feed(Input.El(input))
            state = newState
          } else {
            done = true
          }
        }
        Future.successful(it)
      } catch {
        case NonFatal(e) => Future.failed(e)
      }
    }
  }

  /**
   * A channel for imperative style feeding of input into one or more iteratees.
   */
  trait Channel[E] {

    /**
     * Push an input chunk into this channel
     *
     * @param chunk The chunk to push
     */
    def push(chunk: Input[E])

    /**
     * Push an item into this channel
     *
     * @param item The item to push
     */
    def push(item: E) { push(Input.El(item)) }

    /**
     * Send a failure to this channel.  This results in any promises that the enumerator associated with this channel
     * produced being redeemed with a failure.
     *
     * @param e The failure.
     */
    def end(e: Throwable)

    /**
     * End the input for this channel.  This results in any promises that the enumerator associated with this channel
     * produced being redeemed.
     *
     * Note that an EOF won't be sent, so any iteratees consuming this channel will still be able to consume input
     * (if they are in the cont state).
     */
    def end()

    /**
     * Send an EOF to the channel, and then end the input for the channel.
     */
    def eofAndEnd() {
      push(Input.EOF)
      end()
    }
  }

  /**
   * Create an enumerator and channel for broadcasting input to many iteratees.
   *
   * This is intended for imperative style push input feeding into iteratees.  For example:
   *
   * {{{
   * val (chatEnumerator, chatChannel) = Concurrent.broadcast[String]
   * val chatClient1 = Iteratee.foreach[String](m => println("Client 1: " + m))
   * val chatClient2 = Iteratee.foreach[String](m => println("Client 2: " + m))
   * chatEnumerator |>>> chatClient1
   * chatEnumerator |>>> chatClient2
   *
   * chatChannel.push(Message("Hello world!"))
   * }}}
   */
  def broadcast[E](implicit ec: ExecutionContext): (Enumerator[E], Channel[E]) = {

    import scala.concurrent.stm._

    val iteratees: Ref[List[(Iteratee[E, _], Promise[Iteratee[E, _]])]] = Ref(List[(Iteratee[E, _], Promise[Iteratee[E, _]])]())

    def step(in: Input[E]): Iteratee[E, Unit] = {
      val interested = iteratees.single.swap(List())

      val ready = interested.map {
        case (it, p) =>
          Try {
            it.fold {
              case Step.Done(a, e) => Left(Done(a, e))
              case Step.Cont(k) => {
                val next = k(in)
                next.fold {
                  case Step.Done(a, e) => Left(Done(a, e))
                  case Step.Cont(nestedNext) => Right((Cont(nestedNext), p))
                  case Step.Error(msg, e) => Left(Error(msg, e))
                }
              }
              case Step.Error(msg, e) => Left(Error(msg, e))
            }
          }.map {
            case Left(s) =>
              p.success(s)
              None
            case Right(s) =>
              Some(s)
          }.recover {
            case e: Throwable =>
              p.failure(e)
              None
          }.get
      }

      atomic { implicit txn =>
        iteratees.transform(itList => ready.collect { case Some(s) => s } ++ itList)
      }

      if (in == Input.EOF) Done((), Input.Empty) else Cont(step)

    }

    val redeemed = Ref(None: Option[Try[Unit]])

    val enumerator = new Enumerator[E] {

      def apply[A](it: Iteratee[E, A]): Future[Iteratee[E, A]] = {
        val result = Promise[Iteratee[E, A]]()

        val finished = atomic { implicit txn =>
          redeemed() match {
            case None =>
              iteratees.transform(_ :+ ((it, (result: Promise[Iteratee[E, A]]).asInstanceOf[Promise[Iteratee[E, _]]])))
              None
            case Some(notWaiting) => Some(notWaiting)
          }
        }
        finished.foreach {
          case Success(_) => result.success(it)
          case Failure(e) => result.failure(e)
        }
        result.future
      }

    }

    val mainIteratee = Ref(Future.successful(Cont(step)))

    val toPush = new Channel[E] {

      def push(chunk: Input[E]) {

        val itPromise = Promise[Iteratee[E, Unit]]()

        val current: Future[Iteratee[E, Unit]] = mainIteratee.single.swap(itPromise.future)

        current.map { mainIt =>
          mainIt.fold {
            case Step.Done(a, e) => Done(a, e)
            case Step.Cont(k) => k(chunk)
            case Step.Error(msg, e) => Error(msg, e)
          }
        } onComplete {
          case Success(it) => itPromise.success(it)
          case Failure(e) => {
            val its = atomic { implicit txn =>
              redeemed() = Some(Failure(e))
              iteratees.swap(List())
            }
            itPromise.failure(e)
            its.foreach { case (it, p) => p.success(it) }
          }
        }
      }

      def end(e: Throwable) {
        val current: Future[Iteratee[E, Unit]] = mainIteratee.single.swap(Future.successful(Done((), Input.Empty)))
        def endEveryone() = {
          val its = atomic { implicit txn =>
            redeemed() = Some(Failure(e))
            iteratees.swap(List())
          }
          its.foreach { case (it, p) => p.failure(e) }
        }

        current.onComplete {
          case _ => endEveryone()
        }
      }

      def end() {
        val current: Future[Iteratee[E, Unit]] = mainIteratee.single.swap(Future.successful(Done((), Input.Empty)))
        def endEveryone() = {
          val its = atomic { implicit txn =>
            redeemed() = Some(Success(()))
            iteratees.swap(List())
          }
          its.foreach { case (it, p) => p.success(it) }
        }
        current.onComplete { case _ => endEveryone() }
      }

    }
    (enumerator, toPush)
  }


}
