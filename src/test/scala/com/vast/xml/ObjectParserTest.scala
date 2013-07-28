package com.vast.xml

import org.scalatest.WordSpec
import com.typesafe.scalalogging.slf4j.Logging
import play.api.libs.iteratee.Iteratee
import scala.concurrent.ExecutionContext.Implicits.global

class ObjectParserTest extends WordSpec with AsyncSupport with Logging {

  import ObjectParser._
  import Iteratees._


  "The object parser" should {
    "read values" in {

      val bytes =
        """
          |<simpleObject>
          | <stringField>foo</stringField>
          | <intField>1</intField>
          | <doubleField>3.14</doubleField>
          |</simpleObject>
        """.stripMargin.getBytes

      val it = parseObject(
        "stringField" -> textOnly,
        "intField" -> textOnly,
        "doubleField" -> textOnly
      ).transform(Iteratee.getChunks)

      val parser = InputHandlers.asyncParser(document(it))
      expectResult(List(Some("foo"), Some("1"), Some("3.14"))) {
        blockOnResult(parser(asyncFeedInput(bytes)))
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

      val it = parseObject(
        "stringField" -> textOnly,
        "intField" -> textOnly,
        "doubleField" -> required(textOnly)
      ).transform(Iteratee.getChunks)

      val parser = InputHandlers.asyncParser(document(it))
      intercept[RuntimeException] {
          blockOnResult(parser(asyncFeedInput(bytes)))
      }

    }

  }



}
