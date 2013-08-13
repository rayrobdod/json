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
class ToArrayListTest extends FunSpec
{
	object ToIntDecoder extends JSONDecoder[Int] {
		def decode(s:String) = s.toInt
	}
	
	describe ("ToArrayList") {
	}
	
	describe ("JSONParser.parse(ToArrayList)") {
		ignore ("??? if string is empty") {
			val l = new ToArrayList(ToIntDecoder)
			intercept[ParseException] { JSONParser.parse(l, "") }
		}
		
		it ("should error for wrong brackets (start)") {
			val l = new ToArrayList(ToIntDecoder)
			intercept[ParseException] { JSONParser.parse(l, "{") }
		}
		
		it ("should error for wrong brackets (end)") {
			val l = new ToArrayList(ToIntDecoder)
			intercept[ParseException] { JSONParser.parse(l, "[}") }
		}
		
		it ("should accept an empty array") {
			val l = new ToArrayList(ToIntDecoder)
			JSONParser.parse(l, "[]")
			l.getArrayList.isEmpty
		}
		
		it ("should accept a one-element array") {
			val l = new ToArrayList(ToIntDecoder)
			JSONParser.parse(l, "[1]")
			expectResult(JList(1))(l.getArrayList)
		}
		
		it ("should accept a many-element array") {
			val l = new ToArrayList(ToIntDecoder)
			JSONParser.parse(l, "[1,2,3,4,5]")
			expectResult(JList(1,2,3,4,5))(l.getArrayList)
		}
		
	}
}
