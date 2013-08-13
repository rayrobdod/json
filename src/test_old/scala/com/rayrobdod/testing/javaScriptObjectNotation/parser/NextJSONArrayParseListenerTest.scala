package com.rayrobdod.testing.javaScriptObjectNotation

import com.rayrobdod.javaScriptObjectNotation.parser._
import com.rayrobdod.javaScriptObjectNotation.javaCollection.parseListeners._
import org.scalatest.{FunSuite, BeforeAndAfter, Ignore}
import java.text.ParseException;

/**
 * @author Raymond Dodge
 * @version 14 Jan 2012
 */
class NextJSONArrayParseListenerTest extends FunSuite with BeforeAndAfter
{
	var listener:NextJSONArrayParseListener = _
	
	before
	{
		listener = new NextJSONArrayParseListener()
	}
	
	// making this true means no leniency in regards
	// to "you have an extra comma, dope!"
	test("An empty JSON Object is parsed for indexies")
	{
		val parsed = "[]"
		
		intercept[ParseException] {
			JSONParser.parse(listener, parsed)
		}
	}
	
	test("A single string element JSONObject is parsed for indexies")
	{
		val parsed = "{\"Hello\":\"World\"}"
		
		intercept[ParseException] {
			JSONParser.parse(listener, parsed)
		}
	}
	
	test("A single string element JSONArray is parsed for indexies")
	{
		val parsed = "[\"Hello\"]"
		
		JSONParser.parse(listener, parsed)
		assert(listener.getUnparsedElement() === "\"Hello\"")
		assert(listener.getStartCommaIndex() === 0)
		assert(listener.getEndCommaIndex() === 8)
	}
	
	test("A single int element JSONArray is parsed for indexies")
	{
		val parsed = "{1}"
		
		JSONParser.parse(listener, parsed, 0)
		assert(listener.getUnparsedElement() === "1")
		assert(listener.getStartCommaIndex === 0)
		assert(listener.getEndCommaIndex === 2)
	}
	
	test("A single int element JSONObject is parsed for indexies while skipping to 0")
	{
		val parsed = "{1}"
		
		JSONParser.parse(listener, parsed, 0)
		assert(listener.getUnparsedElement() === "1")
		assert(listener.getStartCommaIndex === 0)
		assert(listener.getEndCommaIndex === 2)
	}
	
	test("A single int element JSONObject with whitespace is parsed for indexies")
	{
		val parsed = "  {  1  }  "
		
		JSONParser.parse(listener, parsed)
		assert(listener.getUnparsedElement() === "  1  ")
		assert(listener.getStartCommaIndex === 2)
		assert(listener.getEndCommaIndex === 8)
	}
	
	/* test("A multiple element JSONObject is parsed for indexies")
	{
		val parsed = "{\"1\":1,\"2\":2,\"3\":3,\"4\":4,\"5\":5]"
		
		JSONParser.parse(listener, parsed)
		JSONParser.parse(listener, parsed)
		assert(listener.getElementStartIndex === 0)
		assert(listener.getKeyValueSplitIndex === 4)
		assert(listener.getElementEndIndex === 6)
		assert(listener.getElementStartIndex === 18)
		assert(listener.getKeyValueSplitIndex === 22)
		assert(listener.getElementEndIndex === 24)
	} */
	
	test("A multiple element JSONArray is parsed for indexies")
	{
		val parsed = "[1,2,3,4,5]"
		
		JSONParser.parse(listener, parsed)
		assert(listener.getUnparsedElement() === "1")
		assert(listener.getStartCommaIndex === 0)
		assert(listener.getEndCommaIndex === 2)
		JSONParser.parse(listener, parsed, listener.getEndCommaIndex - 1)
		JSONParser.parse(listener, parsed, listener.getEndCommaIndex - 1)
		JSONParser.parse(listener, parsed, listener.getEndCommaIndex - 1)
		assert(listener.getUnparsedElement() === "4")
		assert(listener.getStartCommaIndex === 6)
		assert(listener.getEndCommaIndex === 8)
	}
	
	// ignored because apparently it doesn't end
	ignore("A single object element JSONObject is parsed for indexies")
	{
		val parsed = "[[1,2,3]]"
		
		JSONParser.parse(listener, parsed)
		assert(listener.getUnparsedElement() === "[1,2,3]")
		assert(listener.getStartCommaIndex === 0)
		assert(listener.getEndCommaIndex === 8)
	}
}
