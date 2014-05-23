package com.vast.xml

import com.fasterxml.aalto.AsyncXMLStreamReader
import com.fasterxml.aalto.stax.InputFactoryImpl
import java.io.ByteArrayInputStream
import javax.xml.stream.{XMLStreamReader, XMLInputFactory, Location}

import com.vast.util.iteratee._

import scala.concurrent.{ExecutionContext, Future}
import com.typesafe.scalalogging.slf4j.LazyLogging
import scala.util.control.NonFatal
import org.codehaus.stax2.{XMLStreamReader2, XMLInputFactory2}

/**
 * A set of [[com.vast.util.iteratee.Enumerator]]s and [[com.vast.util.iteratee.Enumeratee]]s that assist with parsing
 * streams of Bytes into [[com.vast.xml.XMLEvent]]s.
 *
 * @author David Pratt (dpratt@vast.com)
 */
object InputHandlers extends LazyLogging {

  def syncTransformInput: Enumeratee[Array[Byte], XMLEvent] = new Enumeratee[Array[Byte], XMLEvent] {
    def applyOn[A](inner: Iteratee[XMLEvent, A]): Iteratee[Array[Byte], Iteratee[XMLEvent, A]] = {

      def step(inner: Iteratee[XMLEvent, A])(input: Input[Array[Byte]]): Iteratee[Array[Byte], Iteratee[XMLEvent, A]] = input match {
        case Input.Empty =>
          Cont(step(inner))
        case Input.EOF =>
          Done(inner.feed(Input.EOF))
        case Input.El(bytes) =>
          val parser = xmlFactory.createXMLStreamReader(new ByteArrayInputStream(bytes)).asInstanceOf[XMLStreamReader2]
          var it = inner
          //stax starts on the first event, not before it. Sigh
          val firstInput = XMLEvent(parser)
          it = it.feed(Input.El(firstInput))

          while (parser.hasNext && !it.isDoneOrError) {
            parser.next()
            if(!parser.isWhiteSpace) {
              val input = XMLEvent(parser)
              it = it.feed(Input.El(input))
            }
          }
          parser.closeCompletely()

          it.fold {
            case Step.Error(t, remaining) => Error(t, Input.Empty)
            case _ => Done(it)
          }
      }
      Cont(step(inner))
    }
  }


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
        var it = i
        while (parser.hasNext && parser.next() != AsyncXMLStreamReader.EVENT_INCOMPLETE && !it.isDoneOrError) {
          val input = XMLEvent(parser)
          it = it.feed(Input.El(input))
        }
        it
      }


      def step(it: Iteratee[XMLEvent, A], parser: AsyncXMLStreamReader)(input: Input[Array[Byte]]): Iteratee[Array[Byte], Iteratee[XMLEvent, A]] = {
        input match {
          case Input.Empty =>
            Cont(step(it, parser))
          case Input.EOF =>
            parser.getInputFeeder.endOfInput()
            val res = Done[Array[Byte], Iteratee[XMLEvent, A]](runIteratee(it, parser))
            parser.closeCompletely()
            res
          case Input.El(bytes) =>
            parser.getInputFeeder.feedInput(bytes, 0, bytes.length)
            //run the inner
            runIteratee(it, parser).fold {
              case x@Step.Done(a, remaining) => parser.closeCompletely(); Done(x.it)
              case x@Step.Cont(_) => Cont(step(x.it, parser))
              case x@Step.Error(msg, remaining) => parser.closeCompletely(); Error(msg, Input.Empty)
            }
        }
      }
      Cont { input =>
        //doing it this way ensures that the resulting Iteratee is stateless and can be re-used
        //this only creates the XML parser on demand.
        step(inner, xmlFactory.createAsyncXMLStreamReader())(input)
      }
    }

  }

  /**
   * Synchronously enumerate an XML document.
   */
  def enumerateBytes(bytes: Array[Byte])(implicit ec: ExecutionContext): Enumerator[XMLEvent] = new Enumerator[XMLEvent] {
    def apply[A](i: Iteratee[XMLEvent, A]): Future[Iteratee[XMLEvent, A]] = {
      Future {
        val parser = xmlFactory.createXMLStreamReader(new ByteArrayInputStream(bytes)).asInstanceOf[XMLStreamReader2]
        var it = i
        //woodstox starts on the first event, not before it. Sigh
        val firstInput = XMLEvent(parser)
        it = it.feed(Input.El(firstInput))

        while (parser.hasNext && !it.isDoneOrError) {
          parser.next()
          val input = XMLEvent(parser)
          it = it.feed(Input.El(input))
        }
        parser.closeCompletely()
        it
      }
    }
  } andThen Enumerator.eof

  def asyncParser[A](it: Iteratee[Array[Byte], A])(implicit ec: ExecutionContext): (Enumerator.Channel[Array[Byte]], Future[A]) = {
    //we can share a single instance of the Enumeratee across many invocations, since it's stateless at start
    val (enum, channel) = Enumerator.broadcast[Array[Byte]]
    val eventuallyResult = enum.run(it)

    channel -> eventuallyResult
  }

  def syncParser[A](it: Iteratee[XMLEvent, A])(implicit ec: ExecutionContext): Array[Byte] => Future[A] = {
    inputBytes => {
      Enumerator.enumInput(Input.El(inputBytes)).run(syncTransformInput.transform(it))
    }
  }

  private[this] val xmlFactory = {
    //explicitly create an aalto input factory
    val x = new InputFactoryImpl()
    x.setProperty(XMLInputFactory.IS_COALESCING, false)
    x.setProperty(XMLInputFactory.IS_VALIDATING, false)
    x.setProperty(XMLInputFactory.SUPPORT_DTD, false)
    x.setProperty(XMLInputFactory2.P_PRESERVE_LOCATION, false)
    x.setProperty(XMLInputFactory2.P_AUTO_CLOSE_INPUT, true)
    x.configureForSpeed()
    x
  }
}