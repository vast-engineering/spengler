package com.vast.xml

import org.scalatest.WordSpec
import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.io.IOUtils

import com.vast.util.iteratee._
import scala.xml.NodeSeq

class ObjectParserTest extends WordSpec with AsyncSupport with LazyLogging {

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
      Console.println(blockOnResult(asyncFeedInput(bytes, ComplexObjectParser.parser)))
      logger.debug("ending parse")
    }
  }
}


object ComplexObjectParser {

  case class ComplexObject(count: Int = 0, children: Seq[Entry] = Seq())

  case class Entry(id: String, name: String, price: Int)
  object Entry {
    def apply(ns: NodeSeq): Entry = {

      val id = (ns \ "id").text
      val name = (ns \ "name").text
      val price = (ns \ "price").text.toInt
      Entry(id, name, price)
    }
  }

  import ObjectParser._
  import Iteratees._

  def parser = document(resultParser)

  private[this] def resultParser =
    parseObject(
      "totalResults" -> xmlNumber.map(value => { res: ComplexObject => res.copy(count = value.get.toInt) }),
      "entry" -> NodeSeqParser.parseNodeSeq.map(Entry(_)).map(value => (res: ComplexObject) => res.copy(children = res.children :+ value))
    ).transform(updater(ComplexObject()))


}
