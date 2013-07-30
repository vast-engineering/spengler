package com.vast.xml

import com.fasterxml.aalto.AsyncXMLStreamReader
import com.fasterxml.aalto.stax.InputFactoryImpl
import java.io.ByteArrayInputStream
import javax.xml.stream.{XMLInputFactory, Location}

import com.vast.util.iteratee._

import scala.concurrent.{ExecutionContext, Future}
import com.typesafe.scalalogging.slf4j.Logging
import scala.util.control.NonFatal

/**
 *
 * @author David Pratt (dpratt@vast.com)
 */
object InputHandlers extends Logging {

  /**
   * Synchronously enumerate an XML document.
   */
  def enumerateBytes(bytes: Array[Byte])(implicit ec: ExecutionContext): Enumerator[XMLEvent] = new Enumerator[XMLEvent] {
    def apply[A](i: Iteratee[XMLEvent, A]): Future[Iteratee[XMLEvent, A]] = {
      try {
        val parser = xmlFactory.createXMLStreamReader(new ByteArrayInputStream(bytes))
        var it = i
        //woodstox starts on the first event, not before it. Sigh
        val firstInput = XMLEvent(parser)
        it = it.feed(Input.El(firstInput))

        while (parser.hasNext && !it.isDoneOrError) {
          parser.next()
          val input = XMLEvent(parser)
          it = it.feed(Input.El(input))
        }
        parser.close()
        Future.successful(it)
      } catch {
        case NonFatal(t) => Future.failed(t)
      }
    }
  } andThen Enumerator.eof


  /**
   * Returns an Enumeratee that asynchronously adapts a stream of Array[Byte] into a stream of XMLEvent objects.
   */
  def asyncTransformInput: Enumeratee[Array[Byte], XMLEvent] = new Enumeratee[Array[Byte], XMLEvent] {

    /**
     * Create a new Iteratee that feeds its input, potentially modifying it along the way, into the inner Iteratee, and
     * produces that Iteratee as its result.
     */
    def applyOn[A](inner: Iteratee[XMLEvent, A]): Iteratee[Array[Byte], Iteratee[XMLEvent, A]] = {

      def runIteratee(i: Iteratee[XMLEvent, A], parser: AsyncXMLStreamReader): Iteratee[XMLEvent, A] = {

        def handleError(cause: Throwable, remainingInput: Input[XMLEvent]) = {
          val locationText = locationToString(parser.getLocation)
          val exception = if (!locationText.isEmpty) {
            new IterateeException(s"Error parsing located at $locationText", cause)
          } else {
            cause
          }
          Error(exception, remainingInput)
        }

        def mapErrors(toMap: Iteratee[XMLEvent, A]): Iteratee[XMLEvent, A] = {
          toMap.flatFold {
            case Step.Error(msg, remaining) => handleError(msg, remaining)
            case x => x.it
          }
        }

        var it = i
        while (parser.hasNext && parser.next() != AsyncXMLStreamReader.EVENT_INCOMPLETE && !it.isDoneOrError) {
          val input = XMLEvent(parser)
          it = it.feed(Input.El(input))
        }
        mapErrors(it)
      }


      def step(it: Iteratee[XMLEvent, A], parser: AsyncXMLStreamReader)(input: Input[Array[Byte]]): Iteratee[Array[Byte], Iteratee[XMLEvent, A]] = {
        input match {
          case Input.Empty =>
            Cont(step(it, parser))
          case Input.EOF =>
            parser.getInputFeeder.endOfInput()
            Done(runIteratee(it, parser))
          case Input.El(bytes) =>
            parser.getInputFeeder.feedInput(bytes, 0, bytes.length)
            //run the inner
            runIteratee(it, parser).fold {
              case x@Step.Done(a, remaining) => Done(x.it)
              case x@Step.Cont(_) => Cont(step(x.it, parser))
              case x@Step.Error(msg, remaining) => Error(msg, Input.Empty)
            }
        }
      }
      logger.debug("Creating async parser")
      Cont(step(inner, xmlFactory.createAsyncXMLStreamReader()))
    }

  }

  /**
   * Transform an iteratee from an Iteratee[XMLEvent, A] to Iteratee[Array[Byte], A]
   */
  def transformParser[A](it: Iteratee[XMLEvent, A]) = {
    asyncTransformInput.transform(it)
  }

  def asyncParser[A](it: Iteratee[XMLEvent, A])(implicit ec: ExecutionContext): (Enumerator.Channel[Array[Byte]], Future[A]) = {
    //we can share a single instance of the Enumeratee across many invocations, since it's stateless at start
    val enumeratee = asyncTransformInput
    val (enum, channel) = Enumerator.broadcast[Array[Byte]]
    val eventuallyResult = enum.through(enumeratee).run(it)

    channel -> eventuallyResult
  }

  def syncParser[A](it: Iteratee[XMLEvent, A])(implicit ec: ExecutionContext): Array[Byte] => Future[A] = {
    inputBytes => {
      enumerateBytes(inputBytes).run(it)
    }
  }

  private[this] val xmlFactory = {
    //explicitly create an aalto input factory
    val x = new InputFactoryImpl()
    x.setProperty(XMLInputFactory.IS_COALESCING, true)
    x.configureForSpeed()
    x
  }

  private[this] def locationToString(l: Location) = {
    val lineNumber = l.getLineNumber
    val columnNumber = l.getColumnNumber
    val offset = l.getCharacterOffset
    val output = new java.lang.StringBuilder(40)
    if (lineNumber != -1) {
      output.append(s"line $lineNumber ")
      if (columnNumber != -1) {
        output.append(s"column $columnNumber")
      }
    } else if (offset != -1) {
      output.append(s"offset $offset")
    } else {
      output.append("")
    }
    output.toString
  }

}