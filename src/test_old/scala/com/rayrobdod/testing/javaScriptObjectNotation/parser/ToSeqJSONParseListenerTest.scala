package com.rayrobdod.testing.javaScriptObjectNotation

import com.rayrobdod.javaScriptObjectNotation.parser._
import com.rayrobdod.javaScriptObjectNotation.parser.listeners._
import org.scalatest.{FunSuite, BeforeAndAfter}
import com.rayrobdod.javaScriptObjectNotation.javaCollection.JSONArray
import com.rayrobdod.javaScriptObjectNotation.parser.decoders
		.ToJavaObjectJSONDecoder
import com.rayrobdod.javaScriptObjectNotation.parser.decoders
		.ToScalaCollectionJSONDecoder


/**
 * @author Raymond Dodge
 * @version 18 Jan 2012
 */
class ToSeqJSONParseListenerTest extends FunSuite with BeforeAndAfter
{
	var listener:ToSeqJSONParseListener = _
	
	before
	{
		listener = new ToSeqJSONParseListener()
	}
	
	test("An empty JSON Object is parsed")
	{
		val parsed = "{}"
		
		JSONParser.parse(listener, parsed)
		assert(listener.result === Seq.empty)
	}
	
	test("A single string element JSONObject is parsed")
	{
		val parsed = "{\"Hello\":\"World\"}"
		
		JSONParser.parse(listener, parsed)
		assert(listener.resultMap.map{tuple:(Any, Any) => {
				(tuple._1.toString, tuple._2.toString)
			}} === Map( ("Hello", "World") ))
	}
	
	test("A single string element JSONArray is parsed for size")
	{
		val parsed = "[\"Hello\"]"
		
		JSONParser.parse(listener, parsed)
		assert(listener.result.map{_.toString} === Seq("Hello"))
	}
	
	test("A single int element JSONObject is parsed for size")
	{
		val parsed = "{\"!\":1}"
		
		JSONParser.parse(listener, parsed)
		assert(listener.resultMap.map{tuple:(Any, Any) => {
				(tuple._1.toString, tuple._2)
			}} === Map( ("!", 1) ))
	}
	
	test("A multiple element JSONObject is parsed for size")
	{
		val parsed = "{\"1\":1,\"2\":2,\"3\":3,\"4\":4,\"5\":5]"
		
		JSONParser.parse(listener, parsed)
		assert(listener.resultMap.map{tuple:(Any, Any) => {
				(tuple._1.toString, tuple._2)
			}} === Map( ("1", 1), ("2", 2), ("3", 3), ("4", 4), ("5", 5) ))
	}
	
	test("A multiple element JSONArray is parsed for size")
	{
		val parsed = "[1,2,3,4,5]"
		
		JSONParser.parse(listener, parsed)
		assert(listener.result == Seq(1,2,3,4,5))
	}
	
	test("A single object element JSONObject is parsed")
	{
		val parsed = "[[1,2,3]]"
		
		JSONParser.parse(listener, parsed)
		assert(listener.result == Seq(Seq(1,2,3)))
	}
	
	test("A single object element JSONObject is parsed with a different decoder")
	{
		val parsed = "[[1,2,3]]"
		
		val inner = new JSONArray("[1,2,3]");
		val listener = new ToSeqJSONParseListener(ToJavaObjectJSONDecoder)
		
		JSONParser.parse(listener, parsed)
		assert(listener.result == Seq(inner))
	}
	
	test("A single object element JSONObject is parsed with a different different decoder")
	{
		val parsed = "[[1,2,3]]"
		
		val inner = Seq(1,2,3);
		val listener = new ToSeqJSONParseListener(ToScalaCollectionJSONDecoder)
		
		JSONParser.parse(listener, parsed)
		assert(listener.result == Seq(inner))
	}
	
}
