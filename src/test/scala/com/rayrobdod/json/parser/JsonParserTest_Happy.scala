/*
	Copyright (c) 2015-2016, Raymond Dodge
	All rights reserved.
	
	Redistribution and use in source and binary forms, with or without
	modification, are permitted provided that the following conditions are met:
		* Redistributions of source code must retain the above copyright
		  notice, this list of conditions and the following disclaimer.
		* Redistributions in binary form must reproduce the above copyright
		  notice, this list of conditions and the following disclaimer in the
		  documentation and/or other materials provided with the distribution.
		* Neither the name "<PRODUCT NAME>" nor the names of its contributors
		  may be used to endorse or promote products derived from this software
		  without specific prior written permission.
	
	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
	ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
	WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
	DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
	DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
	(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
	LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
	ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
	(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
	SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
package com.rayrobdod.json.parser;

import scala.collection.immutable.Map;
import org.scalatest.FunSpec;
import com.rayrobdod.json.union.JsonValue
import com.rayrobdod.json.union.StringOrInt
import com.rayrobdod.json.builder.MapBuilder;
import com.rayrobdod.json.union.StringOrInt.{Left => SILeft, Right => SIRight}


class JsonParserTest_Happy extends FunSpec {
	
	private val testValues:Seq[(String, String, Map[StringOrInt, Either[Any, JsonValue]], Int)] = Seq(
		("empty array", """[]""", Map.empty, 0),
		("empty array with before whitespace", """  []""", Map.empty, 0),
		("empty array with before byteordermark", """\ufeff[]""", Map.empty, 0),
		("empty array with middle whitespace", """[  ]""", Map.empty, 0),
		("empty array with ending whitespace", """[]  """, Map.empty, 0),
		("empty object", """{}""", Map.empty, 0),
		("empty object with before byteordermark", """\ufeff{}""", Map.empty, 0),
		("empty object with before whitespace", """  {}""", Map.empty, 0),
		("empty object with middle whitespace", """{  }""", Map.empty, 0),
		("empty object with ending whitespace", """{}  """, Map.empty, 0),
		
		("array containing empty string", """[""]""", Map(SIRight(0) -> Right(JsonValue(""))), 0),
		("array containing single char string", """["1"]""", Map(SIRight(0) -> Right(JsonValue("1"))), 0),
		("array containing multi char string", """["abcde"]""", Map(SIRight(0) -> Right(JsonValue("abcde"))), 0),
		("array containing multiple strings", """["a","b","c"]""", Map(SIRight(0) -> Right(JsonValue("a")), SIRight(1) -> Right(JsonValue("b")), SIRight(2) -> Right(JsonValue("c"))), 0),
		
		("array containing positive number", """[24601]""", Map(SIRight(0) -> Right(JsonValue(24601))), 0),
		("array containing positive number (whitespace)", """[ 24601 ]""", Map(SIRight(0) -> Right(JsonValue(24601))), 0),
		("array containing zero", """[0]""", Map(SIRight(0) -> Right(JsonValue(0))), 0),
		("array containing negative number", """[-42]""", Map(SIRight(0) -> Right(JsonValue(-42))), 0),
		("array containing multiple numbers", """[-1,0,1]""", Map(SIRight(0) -> Right(JsonValue(-1)), SIRight(1) -> Right(JsonValue(0)), SIRight(2) -> Right(JsonValue(1))), 0),
		
		("array containing array", """[[[]]]""", Map(SIRight(0) -> Left(Map(SIRight(0) -> Left(Map())))), 2),
		("array containing array (whitespace)", """[ [ [ ] ] ]""", Map(SIRight(0) -> Left(Map(SIRight(0) -> Left(Map())))), 2),
		("array containing array 2", """[[0,1]]""", Map(SIRight(0) -> Left(Map(SIRight(0) -> Right(JsonValue(0)), SIRight(1) -> Right(JsonValue(1))))), 1),
		("array containing object", """[{}]""", Map(SIRight(0) -> Left(Map())), 1),
		
		("object containing empty string", """{"":""}""", Map(SILeft("") -> Right(JsonValue(""))), 0),
		("object containing single char string", """{"a":"b"}""", Map(SILeft("a") -> Right(JsonValue("b"))), 0),
		("object containing multi char string", """{"abcde":"12345"}""", Map(SILeft("abcde") -> Right(JsonValue("12345"))), 0),
		("object containing multi char string (whitespace)", """{ "abcde" : "12345" }""", Map(SILeft("abcde") -> Right(JsonValue("12345"))), 0),
		("object containing multiple string", """{"a":"b","c":"d"}""", Map(SILeft("a") -> Right(JsonValue("b")), SILeft("c") -> Right(JsonValue("d"))), 0),
		
		("object containing positive number", """{"":24601}""", Map(SILeft("") -> Right(JsonValue(24601))), 0),
		("object containing zero", """{"":0}""", Map(SILeft("") -> Right(JsonValue(0))), 0),
		("object containing negative number", """{"":-42}""", Map(SILeft("") -> Right(JsonValue(-42))), 0),
		("object containing multiple numbers", """{"a":-1,"b":0,"c":1}""", Map(SILeft("a") -> Right(JsonValue(-1)), SILeft("b") -> Right(JsonValue(0)), SILeft("c") -> Right(JsonValue(1))), 0),
		("object containing positive number (whitespace)", """{"": 24601 }""", Map(SILeft("") -> Right(JsonValue(24601))), 0),
		
		("object containing float", """{"": 1.2}""", Map(SILeft("") -> Right(JsonValue(scala.math.BigDecimal("1.2")))), 0),
		// leading zeros are required?
		("object containing float (negative)", """{"": -1.2}""", Map(SILeft("") -> Right(JsonValue(scala.math.BigDecimal("-1.2")))), 0),
		("object containing float (exponent)", """{"": 1e5}""", Map(SILeft("") -> Right(JsonValue(1e5))), 0),
		("object containing float (decimal and exponent)", """{"": 1.2e5}""", Map(SILeft("") -> Right(JsonValue(1.2e5))), 0),
		("object containing float (decimal and neg exponent)", """{"": 1.2e-5}""", Map(SILeft("") -> Right(JsonValue(scala.math.BigDecimal("1.2e-5")))), 0),
		
		("object containing array", """{"":[[]]}""", Map(SILeft("") -> Left(Map(SIRight(0) -> Left(Map())))), 2),
		("object containing array (whitespace)", """{"":[ [] ]}""", Map(SILeft("") -> Left(Map(SIRight(0) -> Left(Map())))), 2),
		("object containing array 2", """{"":[0,1]}""", Map(SILeft("") -> Left(Map(SIRight(0) -> Right(JsonValue(0)), SIRight(1) -> Right(JsonValue(1))))), 1),
		("object containing object", """{"":{}}""", Map(SILeft("") -> Left(Map())), 1),
		("object containing object 2", """{"":{"":{}}}""", Map(SILeft("") -> Left(Map(SILeft("") -> Left(Map())))), 2),
		("object containing object (whitespace)", """{ "" : {} }""", Map(SILeft("") -> Left(Map())), 1),
		
		("string containing escape codes (newline)", """["\n"]""", Map(SIRight(0) -> Right(JsonValue("\n"))), 0),
		("string containing escape codes (tab)", """["\t"]""", Map(SIRight(0) -> Right(JsonValue("\t"))), 0),
		("string containing escape codes (backslash)", """["\\"]""", Map(SIRight(0) -> Right(JsonValue("\\"))), 0),
		("string containing escape codes (other)", """["\"\/\b\f\r"]""", Map(SIRight(0) -> Right(JsonValue("\"/\b\f\r"))), 0),
		// that time where I have to perform a 'bobby tables' to force the thing to put unicode escapes into a JSON Parser 
		("string containing escape codes (unicode)", "[\"\\" + "u0123\"]", Map(SIRight(0) -> Right(JsonValue("ģ"))), 0),
		("string containing escape codes (unicode) 2", "[\"\\" + "u221E\"]", Map(SIRight(0) -> Right(JsonValue("\u221E"))), 0),
		("string containing escape codes (unicode) 3", "[\"\\" + "u0041A\"]", Map(SIRight(0) -> Right(JsonValue("AA"))), 0),
		("array containing keyword (true)", """[true]""", Map(SIRight(0) -> Right(JsonValue(true))), 0),
		("array containing keyword (false)", """[false]""", Map(SIRight(0) -> Right(JsonValue(false))), 0),
		("array containing keyword (null)", """[null]""", Map(SIRight(0) -> Right(JsonValue.JsonValueNull)), 0),
		("array containing keyword (null) (whitespace)", """[ null ]""", Map(SIRight(0) -> Right(JsonValue.JsonValueNull)), 0)
		
	)
	
	def nestedBuilder(level:Int):MapBuilder[StringOrInt, JsonValue, Any] = {
		if (level <= 0) {
			MapBuilder.apply
		} else {
			MapBuilder.apply(new MapBuilder.MapChildBuilder(nestedBuilder(level - 1), {x:Map[StringOrInt,Either[Any,JsonValue]] => x:Any}))
		}
	}
	
	
	describe("JsonParser + MapBuilder can decode") {
		testValues.foreach{abc =>
			val (name:String, source2:String, expected:Map[StringOrInt, Either[Any, JsonValue]], nesting:Int) = abc
			
			val parser = nestedBuilder(nesting)
			
			it (name) {
				val source = source2
				val result = new JsonParser().parse(parser, source).fold({x => x},{x => x},{(a,b) => a})
				assertResult(expected){result}
			}
			it (name + " (reader)") {
				val source = new java.io.StringReader(source2)
				val result = new JsonParser().parse(parser, source).fold({x => x},{x => x},{(a,b) => a})
				assertResult(expected){result}
			}
		}
	}
}
