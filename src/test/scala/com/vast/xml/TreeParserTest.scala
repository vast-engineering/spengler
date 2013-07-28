package com.vast.xml

import com.typesafe.scalalogging.slf4j.Logging
import org.scalatest.WordSpec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import javax.xml.stream.XMLStreamException

/**
 *
 * @author David Pratt (dpratt@vast.com)
 */
class TreeParserTest extends WordSpec with AsyncSupport with Logging {

  import TreeParser._

  val goodBytes =
    """
      |<simpleDoc a="b">
      | <firstField firstAttr="firstValue" secondAttr="secondValue">foo</firstField>
      | <secondField thirdAttr="thirdValue">secondValue</secondField>
      |</simpleDoc>
    """.stripMargin.getBytes

  val badBytes =
    """
      |<simpleDoc a="b">
      | <firstField firstAttr="firstValue" secondAttr="secondValue">foo</firstField>
      | <secondField thirdAttr="thirdValue">secondValue</1secondField>
      |</simpleDoc>
    """.stripMargin.getBytes

  val goodBytesParsed = XMLDocument(
    XMLElement("simpleDoc", Map("a" -> "b"), Seq(
      XMLElement("firstField", Map("firstAttr" -> "firstValue", "secondAttr" -> "secondValue"), Seq(), Some("foo")),
      XMLElement("secondField", Map("thirdAttr" -> "thirdValue"), Seq(), Some("secondValue"))
    ), None)
  )

  "The Async XML parser" should {
    "properly parse XML" in {
      val it = xmlDocument
      val parser = InputHandlers.asyncParser(it)
      expectResult(goodBytesParsed) {
        blockOnResult(parser(asyncFeedInput(goodBytes)))
      }
    }

    "Properly signal errors for invalid XML" in {
      val it = xmlDocument
      val parser = InputHandlers.asyncParser(it)
      intercept[XMLStreamException] {
        blockOnResult(parser(asyncFeedInput(badBytes)))
      }
    }
  }

  "The sync XML parser" should {
    "properly parse XML" in {
      val it = xmlDocument
      val parser = InputHandlers.syncParser(it)
      expectResult(goodBytesParsed) {
        blockOnResult(parser(goodBytes))
      }
    }
    "Properly signal errors for invalid XML" in {
      val it = xmlDocument
      val parser = InputHandlers.syncParser(it)
      intercept[XMLStreamException] {
        blockOnResult(parser(badBytes))
      }
    }

  }

  "The async and sync parsers" should {
    "return the same result" in {
      val it = xmlDocument
      val syncParser = InputHandlers.syncParser(it)
      val asyncParser = InputHandlers.asyncParser(it)

      val eventuallyResult = Future.sequence(List[Future[XMLNode]](
        asyncParser(asyncFeedInput(goodBytes)),
        syncParser(goodBytes)
      )).map { results =>
        assert(results.size === 2)
        val first = results.head
        val second = results.tail.head
        assert(first === second)
      }
      blockOnResult(eventuallyResult)
    }
  }

}
