package com.vast.xml

import play.api.libs.iteratee.Concurrent.Channel
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit

/**
 *
 * @author David Pratt (dpratt@vast.com)
 */
trait AsyncSupport {

  /**
   * Asynchronously feed the input, one byte at a time, into the Channel.
   */
  def asyncFeedInput(bytes: Array[Byte])(feed: Channel[Array[Byte]]): Unit = {
    bytes.foreach { byte =>
      feed.push(Array(byte))
    }
    feed.eofAndEnd()
  }

  def blockOnResult[A](f: Future[A]): A = Await.result(f, Duration(30, TimeUnit.SECONDS))

}
