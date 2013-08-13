package com.rayrobdod.testing.javaScriptObjectNotation

import com.rayrobdod.javaScriptObjectNotation.parser._
import com.rayrobdod.javaScriptObjectNotation.parser.listeners._
import org.scalatest.{FunSuite, BeforeAndAfter}
import java.text.ParseException;

/**
 * @author Raymond Dodge
 * @version 13 Jan 2012
 */
class GetJSONArrayParseListenerTest extends FunSuite with BeforeAndAfter
{
	var listener0:GetJSONArrayParseListener = _
	var listener3:GetJSONArrayParseListener = _
	
	before
	{
		listener0 = new GetJSONArrayParseListener(0)
		listener3 = new GetJSONArrayParseListener(3)
	}
	
	// making this true means no leniency in regards
	// to "you have an extra comma, dope!"
	test("An empty JSON Object is parsed for indexies")
	{
		val parsed = "[]"
		
		intercept[ParseException] {
			JSONParser.parse(listener0, parsed)
		}
	}
	
	/* test("An empty JSON Object with whitespace is parsed for indexies")
	{
		val parsed = "  {  }  "
		
		JSONParser.parse(listener0, parsed)
		assert(listener0.getElementStartIndex === 2)
		assert(listener0.getKeyValueSplitIndex === -1)
		assert(listener0.getElementEndIndex === 5)
	} */
	
	test("A single string element JSONObject is parsed for indexies")
	{
		val parsed = "{\"Hello\":\"World\"}"
		
		intercept[ParseException] {
			JSONParser.parse(listener0, parsed)
		}
	}
	
	test("A single string element JSONArray is parsed for indexies")
	{
		val parsed = "[\"Hello\"]"
		
		JSONParser.parse(listener0, parsed)
		assert(listener0.getUnparsedElement() === "\"Hello\"")
	}
	
	test("A single int element JSONObject is parsed for indexies")
	{
		val parsed = "{1}"
		
		JSONParser.parse(listener0, parsed)
		assert(listener0.getUnparsedElement() === "1")
	}
	
	/* test("A multiple element JSONObject is parsed for indexies")
	{
		val parsed = "{\"1\":1,\"2\":2,\"3\":3,\"4\":4,\"5\":5]"
		
		JSONParser.parse(listener0, parsed)
		JSONParser.parse(listener3, parsed)
		assert(listener0.getElementStartIndex === 0)
		assert(listener0.getKeyValueSplitIndex === 4)
		assert(listener0.getElementEndIndex === 6)
		assert(listener3.getElementStartIndex === 18)
		assert(listener3.getKeyValueSplitIndex === 22)
		assert(listener3.getElementEndIndex === 24)
	} */
	
	test("A multiple element JSONArray is parsed for indexies")
	{
		val parsed = "[1,2,3,4,5]"
		
		JSONParser.parse(listener0, parsed)
		JSONParser.parse(listener3, parsed)
		assert(listener0.getUnparsedElement() === "1")
		assert(listener3.getUnparsedElement() === "4")
	}
	
	test("A single object element JSONObject is parsed for indexies")
	{
		val parsed = "[[1,2,3]]"
		
		JSONParser.parse(listener0, parsed)
		assert(listener0.getUnparsedElement() === "[1,2,3]")
	}
}
