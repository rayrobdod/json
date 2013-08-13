package com.rayrobdod.testing.javaScriptObjectNotation

import com.rayrobdod.javaScriptObjectNotation.parser._
import com.rayrobdod.javaScriptObjectNotation.parser.listeners._
import org.scalatest.{FunSuite, BeforeAndAfter}

/**
 * @author Raymond Dodge
 * @version 13 Jan 2012
 */
class SizeJSONParseListenerTest extends FunSuite with BeforeAndAfter
{
	var listener:SizeJSONParseListener = _
	
	before
	{
		listener = new SizeJSONParseListener()
	}
	
	
	test("An empty JSON Object is parsed for size")
	{
		val parsed = "{}"
		
		JSONParser.parse(listener, parsed)
		assert(listener.getCount == 0)
	}
	
	test("A single string element JSONObject is parsed for size")
	{
		val parsed = "{\"Hello\":\"World\"}"
		
		JSONParser.parse(listener, parsed)
		assert(listener.getCount == 1)
	}
	
	test("A single string element JSONArray is parsed for size")
	{
		val parsed = "[\"Hello\"]"
		
		JSONParser.parse(listener, parsed)
		assert(listener.getCount == 1)
	}
	
	test("A single int element JSONObject is parsed for size")
	{
		val parsed = "{\"!\":1}"
		
		JSONParser.parse(listener, parsed)
		assert(listener.getCount == 1)
	}
	
	test("A multiple element JSONObject is parsed for size")
	{
		val parsed = "{\"1\":1,\"2\":2,\"3\":3,\"4\":4,\"5\":5]"
		
		JSONParser.parse(listener, parsed)
		assert(listener.getCount == 5)
	}
	
	test("A multiple element JSONArray is parsed for size")
	{
		val parsed = "[1,2,3,4,5]"
		
		JSONParser.parse(listener, parsed)
		assert(listener.getCount == 5)
	}
	
	test("A single object element JSONObject is parsed for size")
	{
		val parsed = "[[1,2,3]]"
		
		JSONParser.parse(listener, parsed)
		assert(listener.getCount == 1)
	}
}
