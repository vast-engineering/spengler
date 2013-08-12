package com.vast.xml

import org.scalatest.WordSpec
import com.typesafe.scalalogging.slf4j.Logging
import org.apache.commons.io.IOUtils
import scala.concurrent.ExecutionContext
import play.api.libs.json.JsObject

import scala.concurrent.ExecutionContext.Implicits.global

import com.vast.util.iteratee._

class ObjectParserTest extends WordSpec with AsyncSupport with Logging {

  import ObjectParser._
  import Iteratees._

  "The object parser" should {

    val it = document(parseObject(
      "stringField" -> textOnly,
      "intField" -> textOnly,
      "doubleField" -> required(textOnly)
    ).transform(Iteratee.getChunks))

    "read values" in {

      val bytes =
        """
          |<simpleObject>
          | <stringField>foo</stringField>
          | <intField>1</intField>
          | <doubleField>3.14</doubleField>
          |</simpleObject>
        """.stripMargin.getBytes

      expectResult(List(Some("foo"), Some("1"), "3.14")) {
        blockOnResult(asyncFeedInput(bytes, it))
      }
    }

    "report a missing value" in {

      val bytes =
        """
          |<simpleObject>
          | <stringField>foo</stringField>
          | <intField>1</intField>
          | <doubleField></doubleField>
          |</simpleObject>
        """.stripMargin.getBytes

      intercept[IterateeException] {
        blockOnResult(asyncFeedInput(bytes, it))
      }
    }

    "parse a complex document" in {
      val bytes = IOUtils.toByteArray(classOf[TreeParserTest].getResourceAsStream("/complexResponse.xml"))
      logger.debug("Starting parse")
      Console.println(blockOnResult(asyncFeedInput(bytes, CarsResultIteratee.carsResultIteratee)))
      logger.debug("ending parse")
    }
  }
}


object CarsResultIteratee {

  case class SearchResult(count: Int = 0, listings: Seq[JsObject] = Seq())
  case class Listing(id: String = "")

  import ObjectParser._
  import Iteratees._

  type Updater[Result] = Result => Result
  type ValueUpdater[Result, V] = V => (Result => Result)

  def carsResultIteratee(implicit ec: ExecutionContext) = document(resultParser)

  private[this] def producer[T](value: T) = {
    Iteratee.fold[Updater[T], T](value) {
      case (result, updater) => updater(result)
    }
  }

  private[this] def resultParser =
    parseObject(
      "totalResults" -> xmlNumber.map(value => { res: SearchResult => res.copy(count = value.get.toInt) }),
      "entry" -> JsValueParser.jsonObject.map(value => (res: SearchResult) => res.copy(listings = res.listings :+ value))
    ).transform(producer(SearchResult()))

}
