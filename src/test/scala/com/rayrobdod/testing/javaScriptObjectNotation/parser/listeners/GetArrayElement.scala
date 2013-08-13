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
class GetArrayElementTest extends FunSpec
{
	object ToIntDecoder extends JSONDecoder[Int] {
		def decode(s:String) = s.toInt
	}
	
	describe ("GetArrayElement") {
	}
	
	describe ("JSONParser.parse(GetArrayElement)") {
		it ("should error if string is empty") {
			val l = new GetArrayElement(0, ToIntDecoder)
			intercept[ParseException] { JSONParser.parse(l, "") }
		}
		
		it ("should error to get an element of an empty array") {
			val l = new GetArrayElement(0, ToIntDecoder)
			intercept[ParseException] { JSONParser.parse(l, "[]") }
		}
		
		it ("should accept a one-element array") {
			val l = new GetArrayElement(0, ToIntDecoder)
			JSONParser.parse(l, "[1]")
			expectResult(1)(l.getParsedElement)
		}
		
		it ("should error for indexOutOfBounds") {
			val l = new GetArrayElement(3, ToIntDecoder)
			intercept[ParseException] { JSONParser.parse(l, "[1]") }
		}
		
		it ("should get first element of many-element array") {
			val l = new GetArrayElement(0, ToIntDecoder)
			JSONParser.parse(l, "[1,2,3,4,5]")
			expectResult(1)(l.getParsedElement)
		}
		
		it ("should get fourth element of many-element array") {
			val l = new GetArrayElement(3, ToIntDecoder)
			JSONParser.parse(l, "[1,2,3,4,5]")
			expectResult(4)(l.getParsedElement)
		}
		
	}
}
