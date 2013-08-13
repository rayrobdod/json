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
class ToHashMapTest extends FunSpec
{
	object ToIntDecoder extends JSONDecoder[Int] {
		def decode(s:String) = s.toInt
	}
	
	describe ("ToArrayList") {
	}
	
	describe ("JSONParser.parse(ToArrayList)") {
		ignore ("??? if string is empty") {
			val l = new ToHashMap(ToIntDecoder)
			intercept[ParseException] { JSONParser.parse(l, "") }
		}
		
		it ("should error for wrong brackets (start)") {
			val l = new ToHashMap(ToIntDecoder)
			intercept[ParseException] { JSONParser.parse(l, "[") }
		}
		
		it ("should error for wrong brackets (end)") {
			val l = new ToHashMap(ToIntDecoder)
			intercept[ParseException] { JSONParser.parse(l, "{]") }
		}
		
		it ("should accept an empty array") {
			val l = new ToHashMap(ToIntDecoder)
			JSONParser.parse(l, "{}")
			l.getResult.isEmpty
		}
		
		it ("should accept a one-element array") {
			val l = new ToHashMap(ToIntDecoder)
			JSONParser.parse(l, "{\"\":1}")
			expectResult(1)(l.getResult.get(""))
		}
		
		it ("should accept a many-element array") {
			val l = new ToHashMap(ToIntDecoder)
			JSONParser.parse(l, """{"1":1,"2":2,"3":3,"4":4,"5":5}""")
			expectResult(1)(l.getResult.get("1"))
			expectResult(2)(l.getResult.get("2"))
			expectResult(3)(l.getResult.get("3"))
			expectResult(4)(l.getResult.get("4"))
			expectResult(5)(l.getResult.get("5"))
		}
		
	}
}
