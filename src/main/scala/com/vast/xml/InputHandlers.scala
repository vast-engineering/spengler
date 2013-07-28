package com.vast.xml

import com.ctc.wstx.stax.WstxInputFactory
import com.fasterxml.aalto.AsyncXMLStreamReader
import com.fasterxml.aalto.stax.InputFactoryImpl
import java.io.ByteArrayInputStream
import javax.xml.stream.{Location, XMLInputFactory}

import play.api.libs.iteratee.Concurrent.Channel
import play.api.libs.iteratee._

import scala.util.control.NonFatal

import scala.concurrent.{ExecutionContext, Promise, Future}
import scala.util.Failure
import scala.Some
import scala.util.Success


/**
 *
 * @author David Pratt (dpratt@vast.com)
 */
object InputHandlers {


  /**
   * Synchronously enumerate an XML document.
   */
  def enumerateBytes(bytes: Array[Byte]): Enumerator[XMLEvent] = {

    val parser = syncFactory.createXMLStreamReader(new ByteArrayInputStream(bytes))

    //the default Woodstox parser starts at the first event, not before it, so we call next() after we
    //generate the event.
    val enum = Enumerator.unfold(parser) {
      p =>
        if (p.hasNext) {
          val event = Some(p -> XMLEvent(p))
          p.next() //move to the next event
          event
        } else {
          None
        }
    } andThen Enumerator.eof

    enum
  }

  /**
   * Returns an Enumeratee that asynchronously adapts a stream of Array[Byte] into a stream of XMLEvent objects.
   */
  def asyncTransformInput(implicit ec: ExecutionContext): Enumeratee[Array[Byte], XMLEvent] = new Enumeratee[Array[Byte], XMLEvent] {

    def step[A](parser: AsyncXMLStreamReader, k: Input[XMLEvent] => Iteratee[XMLEvent, A])
               (input: Input[Array[Byte]]): Iteratee[Array[Byte], Iteratee[XMLEvent, A]] = {


      def runIteratee(inputBytes: Input[Array[Byte]],
                      genNextState: Input[XMLEvent] => Iteratee[XMLEvent, A]): Iteratee[XMLEvent, A] = {

        def handleError(msg: String, remainingInput: Input[XMLEvent]) = {
          val errorMessage = {
            val locationText = locationToString(parser.getLocation)
            if(!locationText.isEmpty) {
              s"""
                |Error parsing located at $locationText
                |
                |$msg
              """.stripMargin
            } else {
              msg
            }
          }

          Error(errorMessage, remainingInput)
        }

        def mapErrors(toMap: Iteratee[XMLEvent, A]): Iteratee[XMLEvent, A] = {
          toMap.pureFlatFold {
            case Step.Error(msg, remaining) => handleError(msg, remaining)
            case x => x.it
          }
        }

        def runHelper(it: Iteratee[XMLEvent, A]): Iteratee[XMLEvent, A] = {

          //we just feed input manually - this is way faster than composing iteratees and using recursion
          var currentState = it
          while(parser.hasNext && parser.next() != AsyncXMLStreamReader.EVENT_INCOMPLETE) {
            val input = Input.El(XMLEvent(parser))
            currentState = it.pureFlatFold {
              case Step.Cont(genNext) => mapErrors(genNext(input))
              case x => x.it
            }
          }
          currentState
        }

        def feedEOF(it: Iteratee[XMLEvent, A]) = {
          Iteratee.flatten(it.feed(Input.EOF))
        }

        val iteratee = Cont(genNextState)
        inputBytes match {
          case in@Input.El(bytes) =>
            parser.getInputFeeder.feedInput(bytes, 0, bytes.length)
            runHelper(iteratee)
          case in@Input.EOF =>
            parser.getInputFeeder.endOfInput()
            feedEOF(runHelper(iteratee))
          case _ => iteratee //do nothing
        }
      }

      input match {
        case Input.Empty =>
          Cont(step(parser, k))
        case in =>
          runIteratee(in, k).pureFlatFold {
            case Step.Cont(continue) => Cont(step(parser, continue))
            case Step.Error(msg, eventInput) => parser.close(); Error(msg, in)
            case x@Step.Done(a, remaining) => parser.close(); Done(x.it)
          }
      }
    }

    def applyOn[A](inner: Iteratee[XMLEvent, A]): Iteratee[Array[Byte], Iteratee[XMLEvent, A]] = {
      inner.pureFlatFold {
        case Step.Cont(k) => Cont(step(asyncFactory.createAsyncXMLStreamReader, k))
        case _ => Done(inner, Input.Empty)
      }
    }
  }

  def asyncParser[A](it: Iteratee[XMLEvent, A])(implicit ec: ExecutionContext): (Channel[Array[Byte]] => Unit) => Future[A] = {
    //we can share a single instance of the Enumeratee across many invocations, since it's stateless at start
    val enumeratee = asyncTransformInput

    inputHandler => {
      unicast(inputHandler).through(enumeratee).run(it)
    }
  }

  def syncParser[A](it: Iteratee[XMLEvent, A]): Array[Byte] => Future[A] = {
    inputBytes => {
      enumerateBytes(inputBytes).run(it)
    }
  }

  private val syncFactory = {
    val x = new WstxInputFactory
    x.configureForSpeed()
    x.setProperty(XMLInputFactory.IS_NAMESPACE_AWARE, false)
    x.setProperty(XMLInputFactory.IS_COALESCING, true)
    x
  }


  private val asyncFactory = {
    //explicitly create an aalto input factory
    val x = new InputFactoryImpl()
    x.configureForSpeed()
    x.setProperty(XMLInputFactory.IS_NAMESPACE_AWARE, false)
    x.setProperty(XMLInputFactory.IS_COALESCING, true)
    x
  }

  private def locationToString(l: Location) = {
    val lineNumber = l.getLineNumber
    val columnNumber = l.getColumnNumber
    val offset = l.getCharacterOffset
    val output = new StringBuilder
    if(lineNumber != -1) {
      output.append(s"line $lineNumber ")
      if(columnNumber != -1) {
        output.append(s"column $columnNumber")
      }
    } else if(offset != -1) {
      output.append(s"offset $offset")
    } else {
      output.append("")
    }
    output.toString()
  }




  /**
   * Create an enumerator that allows imperative style pushing of input into a single iteratee. This is
   * identical to the default Play unicast Enumerator, but with extra exception handling.
   *
   * The enumerator may be used multiple times, each time will cause a new invocation of `onStart`, which will pass a
   * [[play.api.libs.iteratee.Concurrent.Channel]] that can be used to feed input into the iteratee.  However, note that
   * there is no way for the caller to know which iteratee is finished or encountered an error in the `onComplete` or
   * `onError` functions.
   *
   * @param onStart Called when an enumerator is applied to an iteratee, providing the channel to feed input into that
   *                iteratee.
   * @param onComplete Called when an iteratee is done.
   * @param onError Called when an iteratee encounters an error, supplying the error and the input that caused the error.
   * @return
   */
  def unicast[E](
                  onStart: Channel[E] => Unit,
                  onComplete: => Unit = (),
                  onError: (String, Input[E]) => Unit = (_: String, _: Input[E]) => ())(implicit ec: ExecutionContext) = new Enumerator[E] {

    import scala.concurrent.stm.Ref

    def apply[A](it: Iteratee[E, A]): Future[Iteratee[E, A]] = {
      val promise: scala.concurrent.Promise[Iteratee[E, A]] = Promise[Iteratee[E, A]]()
      val iteratee: Ref[Future[Option[Input[E] => Iteratee[E, A]]]] = Ref(it.pureFold { case  Step.Cont(k) => Some(k); case other => promise.success(other.it); None})

      val pushee = new Channel[E] {
        def close() {
          iteratee.single.swap(Future.successful(None)).onComplete{
            case Success(maybeK) => maybeK.foreach { k =>
              try {
                promise.success(k(Input.EOF))
              } catch {
                case NonFatal(e) => promise.failure(e)
              }
            }
            case Failure(e) => promise.failure(e)
          }
        }

        def end(e: Throwable) {
          iteratee.single.swap(Future.successful(None)).onComplete {
            case Success(maybeK) =>
              maybeK.foreach(_ => promise.failure(e))
            case Failure(error) => promise.failure(error)
          }
        }

        def end() {
          iteratee.single.swap(Future.successful(None)).onComplete { maybeK =>
            maybeK.get.foreach(k => promise.success(Cont(k)))
          }
        }

        def push(item: Input[E]) {
          val eventuallyNext = Promise[Option[Input[E] => Iteratee[E,A]]]()
          iteratee.single.swap(eventuallyNext.future).onComplete {
            case Success(None) => eventuallyNext.success(None)
            case Success(Some(k)) =>
              try {
                val n = {
                  val next = k(item)
                  next.pureFold {
                    case Step.Done(a, in) => {
                      onComplete
                      promise.success(next)
                      None
                    }
                    case Step.Error(msg, e) =>
                      onError(msg, e)
                      promise.success(next)
                      None
                    case Step.Cont(knext) =>
                      Some(knext)
                  }
                }
                eventuallyNext.completeWith(n)
              } catch {
                case NonFatal(e) => promise.failure(e)
              }
            case Failure(e) =>
              promise.failure(e)
              eventuallyNext.success(None)
          }
        }
      }
      onStart(pushee)
      promise.future
    }

  }

}
