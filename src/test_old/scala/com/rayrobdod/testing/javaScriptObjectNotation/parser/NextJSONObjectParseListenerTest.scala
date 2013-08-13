package com.rayrobdod.testing.javaScriptObjectNotation

import com.rayrobdod.javaScriptObjectNotation.parser._
import com.rayrobdod.javaScriptObjectNotation.javaCollection.parseListeners._
import org.scalatest.{FunSuite, BeforeAndAfter}
import java.text.ParseException;

/**
 * @author Raymond Dodge
 * @version 15 Jan 2012
 */
class NextJSONObjectParseListenerTest extends FunSuite with BeforeAndAfter
{
	var listener:NextJSONObjectParseListener = _
	
	before
	{
		listener = new NextJSONObjectParseListener()
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
		
		JSONParser.parse(listener, parsed)
		assert(listener.getUnparsedKey() === "\"Hello\"")
		assert(listener.getUnparsedValue() === "\"World\"")
		assert(listener.getStartCommaIndex() === 0)
		assert(listener.getColonIndex() === 8)
		assert(listener.getEndCommaIndex() === 16)
	}
	
	test("A single string element JSONArray is parsed for indexies")
	{
		val parsed = "[\"Hello\"]"
		
		intercept[ParseException] {
			JSONParser.parse(listener, parsed)
		}
	}
	
	test("A single int element JSONArray is parsed for indexies")
	{
		val parsed = "{\"1\":1}"
		
		JSONParser.parse(listener, parsed)
		assert(listener.getUnparsedKey() === "\"1\"")
		assert(listener.getUnparsedValue() === "1")
		assert(listener.getStartCommaIndex === 0)
		assert(listener.getColonIndex === 4)
		assert(listener.getEndCommaIndex === 6)
	}
	
	test("A single int element JSONObject is parsed for indexies while skipping to 0")
	{
		val parsed = "{\"1\":1}"
		
		JSONParser.parse(listener, parsed, 0)
		assert(listener.getUnparsedKey() === "\"1\"")
		assert(listener.getUnparsedValue() === "1")
		assert(listener.getStartCommaIndex === 0)
		assert(listener.getColonIndex === 4)
		assert(listener.getEndCommaIndex === 6)
	}
	
	test("A single int element JSONObject with whitespace is parsed for indexies")
	{
		val parsed = "  { \"1\" :  1  }  "
		
		JSONParser.parse(listener, parsed)
		assert(listener.getUnparsedKey() === " \"1\" ")
		assert(listener.getUnparsedValue() === "  1  ")
		assert(listener.getStartCommaIndex === 2)
		assert(listener.getColonIndex === 8)
		assert(listener.getEndCommaIndex === 14)
	}
	
	// ignored because apparently it doesn't end
	ignore("A multiple element JSONObject is parsed for indexies")
	{
		val parsed = "{\"1\":1,\"2\":2,\"3\":3,\"4\":4,\"5\":5]"
		
		JSONParser.parse(listener, parsed)
		assert(listener.getUnparsedKey() === "\"1\"")
		assert(listener.getUnparsedValue() === "1")
		assert(listener.getStartCommaIndex === 0)
		assert(listener.getColonIndex === 4)
		assert(listener.getEndCommaIndex === 6)
		JSONParser.parse(listener, parsed, listener.getEndCommaIndex - 1)
		JSONParser.parse(listener, parsed, listener.getEndCommaIndex - 1)
		JSONParser.parse(listener, parsed, listener.getEndCommaIndex - 1)
		assert(listener.getStartCommaIndex === 18)
		assert(listener.getColonIndex === 22)
		assert(listener.getEndCommaIndex === 24)
	}
	
	// ignored because doesn't compile
	/* ignore("A multiple element JSONArray is parsed for indexies")
	{
		val parsed = "[1,2,3,4,5]"
		
		System.out.println("multi-elem test");
		
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
	} */
	
	/* ignored because doesn't compile
	ignore("A single object element JSONObject is parsed for indexies")
	{
		val parsed = "[[1,2,3]]"
		
		JSONParser.parse(listener, parsed)
		assert(listener.getUnparsedElement() === "[1,2,3]")
		assert(listener.getStartCommaIndex === 0)
		assert(listener.getEndCommaIndex === 8)
	} */
}
