package com.vast.xml

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit
import com.vast.util.iteratee.{Input, Iteratee, Enumerator}

import concurrent.ExecutionContext.Implicits.global

/**
 *
 * @author David Pratt (dpratt@vast.com)
 */
trait AsyncSupport {

  /**
   * Asynchronously feed the input, one byte at a time, into the Channel.
   */
  def asyncFeedInput[A](bytes: Array[Byte], it: Iteratee[XMLEvent, A]): Future[A] = {
    val (channel, result) = InputHandlers.asyncParser(it)
    Future {
      channel.push(bytes)
//      bytes.foreach { byte =>
//        channel.push(Array(byte))
//      }
      channel.eofAndEnd()
    }
    result
  }

  def blockOnResult[A](f: Future[A]): A = Await.result(f, Duration(30, TimeUnit.SECONDS))

}
