package com.rayrobdod.testing.javaScriptObjectNotation.parser.listeners

import com.rayrobdod.javaScriptObjectNotation.parser._
import com.rayrobdod.javaScriptObjectNotation.parser.listeners._
import org.scalatest.{FunSpec}
import java.text.ParseException;
import java.util.Arrays.{asList => JList}

/**
 * @author Raymond Dodge
 * @version 2013 Jun 23
 */
class ToScalaCollectionTest extends FunSpec
{
	object ToIntDecoder extends JSONDecoder[java.lang.Integer] {
		def decode(s:String):java.lang.Integer = s.toInt
	}
	
	describe ("ToScalaCollection") {
	}
	
	describe ("JSONParser.parse(ToScalaCollection)") {
		ignore ("should error if string is empty") {
			val l = new ToScalaCollection(ToIntDecoder)
			intercept[ParseException] { JSONParser.parse(l, "") }
		}
		
		it ("should accept an empty array") {
			val l = new ToScalaCollection(ToIntDecoder)
			JSONParser.parse(l, "[]")
			assert(l.result.isEmpty)
		}
		
		it ("should accept a one-element array") {
			val l = new ToScalaCollection(ToIntDecoder)
			JSONParser.parse(l, "[1]")
			expectResult(Seq(1))(l.resultSeq)
		}
		
		it ("should accept a many-element array") {
			val l = new ToScalaCollection(ToIntDecoder)
			JSONParser.parse(l, "[1,2,3,4,5]")
			expectResult(Seq(1,2,3,4,5))(l.resultSeq)
		}
		
		
		
		it ("should accept an empty object") {
			val l = new ToScalaCollection(ToIntDecoder)
			JSONParser.parse(l, "{}")
			assert(l.result.isEmpty)
		}
		
		it ("should accept a one-element object") {
			val l = new ToScalaCollection(ToIntDecoder)
			JSONParser.parse(l, "{\"\":1}")
			expectResult(Map("" -> 1))(l.resultMap)
		}
		
		it ("should accept a many-element object") {
			val l = new ToScalaCollection(ToIntDecoder)
			JSONParser.parse(l, "{\"1\":1,\"2\":2,\"3\":3}")
			expectResult(Map("1" -> 1,"2" -> 2,"3" -> 3))(l.resultMap)
		}
		
		it ("should accept a mixed ... thing") {
			val l = new ToScalaCollection(ToIntDecoder)
			JSONParser.parse(l, "{\"1\":1,2,\"3\":3}")
			expectResult(Seq(Right("1" -> 1), Left(2), Right("3" -> 3)))(l.result)
		}
	}
}
