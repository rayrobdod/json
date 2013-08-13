package com.rayrobdod.testing.javaScriptObjectNotation

import com.rayrobdod.javaScriptObjectNotation.parser._
import com.rayrobdod.javaScriptObjectNotation.parser.listeners._
import org.scalatest.{FunSuite, BeforeAndAfter}

/**
 * @author Raymond Dodge
 * @version 13 Jan 2012
 */
class BoundsJSONParseListenerTest extends FunSuite with BeforeAndAfter
{
	var listener0:BoundsJSONParseListener = _
	var listener3:BoundsJSONParseListener = _
	
	before
	{
		listener0 = new BoundsJSONParseListener(0)
		listener3 = new BoundsJSONParseListener(3)
	}
	
	test("An empty JSON Object is parsed for indexies")
	{
		val parsed = "{}"
		
		JSONParser.parse(listener0, parsed)
		assert(listener0.getElementStartIndex === 0)
		assert(listener0.getKeyValueSplitIndex === -1)
		assert(listener0.getElementEndIndex === 1)
	}
	
	test("An empty JSON Object with whitespace is parsed for indexies")
	{
		val parsed = "  {  }  "
		
		JSONParser.parse(listener0, parsed)
		assert(listener0.getElementStartIndex === 2)
		assert(listener0.getKeyValueSplitIndex === -1)
		assert(listener0.getElementEndIndex === 5)
	}
	
	test("A single string element JSONObject is parsed for indexies")
	{
		val parsed = "{\"Hello\":\"World\"}"
		
		JSONParser.parse(listener0, parsed)
		assert(listener0.getElementStartIndex === 0)
		assert(listener0.getKeyValueSplitIndex === 8)
		assert(listener0.getElementEndIndex === 16)
	}
	
	test("A single string element JSONArray is parsed for indexies")
	{
		val parsed = "[\"Hello\"]"
		
		JSONParser.parse(listener0, parsed)
		
		JSONParser.parse(listener0, parsed)
		assert(listener0.getElementStartIndex === 0)
		assert(listener0.getKeyValueSplitIndex === -1)
		assert(listener0.getElementEndIndex === 8)
	}
	
	test("A single int element JSONObject is parsed for indexies")
	{
		val parsed = "{\"!\":1}"
		
		JSONParser.parse(listener0, parsed)
		assert(listener0.getElementStartIndex === 0)
		assert(listener0.getKeyValueSplitIndex === 4)
		assert(listener0.getElementEndIndex === 6)
	}
	
	test("A multiple element JSONObject is parsed for indexies")
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
	}
	
	test("A multiple element JSONArray is parsed for indexies")
	{
		val parsed = "[1,2,3,4,5]"
		
		JSONParser.parse(listener0, parsed)
		JSONParser.parse(listener3, parsed)
		assert(listener0.getElementStartIndex === 0)
		assert(listener0.getKeyValueSplitIndex === -1)
		assert(listener0.getElementEndIndex === 2)
		assert(listener3.getElementStartIndex === 6)
		assert(listener3.getKeyValueSplitIndex === -1)
		assert(listener3.getElementEndIndex === 8)
	}
	
	test("A single object element JSONObject is parsed for indexies")
	{
		val parsed = "[[1,2,3]]"
		
		JSONParser.parse(listener0, parsed)
		assert(listener0.getElementStartIndex === 0)
		assert(listener0.getKeyValueSplitIndex === -1)
		assert(listener0.getElementEndIndex === 8)
	}
}
