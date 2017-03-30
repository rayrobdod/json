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

import org.scalatest.FunSpec;
import com.rayrobdod.json.union.{JsonValue, StringOrInt, ParserRetVal}
import com.rayrobdod.json.builder._
import com.rayrobdod.json.parser.JsonParser.Failures._
import com.rayrobdod.json.builder.BuilderTest.EnforcedFailure

class JsonParserTest_Unhappy extends FunSpec {
	
	private val parser = new JsonParser()
	private val mapBuilder = MapBuilder[StringOrInt, JsonValue].mapResult{x => x:Any}
	private val seq2Builder = SeqBuilder(PrimitiveSeqBuilder[JsonValue]).mapResult{x => x:Any}
	private val throwBuilder = new ThrowBuilder(EnforcedFailure).mapResult{x => x:Any}
	
	private val failureCases:Seq[(String, Iterable[Char], Builder[StringOrInt, JsonValue, Any, Any], JsonParser.Failures)] = Seq(
		  ("errors when object is incomplete", """{""", mapBuilder, IncompleteObject)
		, ("errors when array is incomplete", "[", mapBuilder, IncompleteObject)
		, ("error idx includes insignificant whitespace", " abc", mapBuilder, UnexpectedChar('a', "'{' or '['", 1))
		, ("errors when string starts with non-space characters", "abc", mapBuilder, UnexpectedChar('a', "'{' or '['", 0))
		, ("errors when map key is not a string", "{2:3}", mapBuilder, UnexpectedChar('2', "start of object key", 1))
		, ("errors when map key is invalid string", """{"\u0001":3}""", mapBuilder, ControlCharInString('\u0001', 2))
		, ("errors when map key is more than just a string", """{"fd" null}""", mapBuilder, UnexpectedChar('n', "':'", 6))
		, ("errors when map value is more than just a string", """{" ":"hello" "world"}""", mapBuilder, UnexpectedChar('"', "',' or '}'", 13))
		, ("errors when array value is more than just a string", """["hello" "world"]""", mapBuilder, UnexpectedChar('"', "',' or ']'", 9))
		, ("errors when object value is not alphanumeric", """{"fd":%%%}""", mapBuilder, UnexpectedChar('%', "start of value", 6))
		, ("errors when array value is not alphanumeric", """[%%%]""", mapBuilder, UnexpectedChar('%', "start of value", 1))
		, ("errors when array value is not a keyword", """[nothing]""", mapBuilder, NotAKeyword("nothing", 1))
		, ("errors when number starts with a decimal point (array)", """[.5]""", mapBuilder, NumericValueStartedWithFullStop(1))
		, ("errors when number starts with a decimal point (object)", """{"":.5}""", mapBuilder, NumericValueStartedWithFullStop(4))
		, ("errors when number starts with an exponent indicator (array)", """[e5]""", mapBuilder, NotAKeyword("e5", 1))
		, ("errors when number starts with a exponent indicator (object)", """{"":e5}""", mapBuilder, NotAKeyword("e5", 4))
		, ("errors when number starts with a plus sign (array)", """[+5]""", mapBuilder, UnexpectedChar('+', "start of value",1))
		, ("errors when number starts with a plus sign (object)", """{"":+5}""", mapBuilder, UnexpectedChar('+', "start of value",4))
		, ("number format (2)", """[51sfd]""", mapBuilder, NotANumber("51sfd", 1))
		, ("number format", """{"":51sfd}""", mapBuilder, NotANumber("51sfd", 4))
		, ("numbers cannot end with a dot", """{"":2.}""", mapBuilder, NotANumber("2.", 4))
		, ("numbers cannot end with an e", """{"":2e}""", mapBuilder, NotANumber("2e", 4))
		, ("numbers cannot be just a hypen-minus", """{"":-}""", mapBuilder, NotANumber("-", 4))
		, ("numbers must have digits between '.' and 'e'", """{"":9.e+1}""", mapBuilder, NotANumber("9.e+1", 4))
		, ("numbers cannot contain a leading zero", """{"":012}""", mapBuilder, NotANumber("012", 4))
		, ("numbers cannot contain a leading zero, even with a leading hyphen-minus", """{"":-012}""", mapBuilder, NotANumber("-012", 4))
		, ("'True' is not a keyword", """{"":True}""", mapBuilder, UnexpectedChar('T', "start of value",4))
		, ("'NaN' is not a keyword", """{"":NaN}""", mapBuilder, UnexpectedChar('N', "start of value",4))
		
		, ("errors on illegal escape character inside string", """["\a"]""", mapBuilder, IllegalEscape('a', 3))
		, ("errors on illegal escape character inside string (obj)", """{"":"\a"}""", mapBuilder, IllegalEscape('a', 6))
		, ("errors on illegal character in unicode escape", "[\"\\u1y34\"]", mapBuilder, NotAUnicodeEscape("1y34", 3))
		, ("errors on illegal character in unicode escape 2", "[\"\\u1Y34\"]", mapBuilder, NotAUnicodeEscape("1Y34", 3))
		, ("errors on illegal character in unicode escape 3", "[\"\\u1 34\"]", mapBuilder, NotAUnicodeEscape("1 34", 3))
		, ("errors on illegal character in unicode escape 4", "[\"\\u1=34\"]", mapBuilder, NotAUnicodeEscape("1=34", 3))
		
		, ("errors on trailing comma (array)", """[1,2,3,]""", mapBuilder, UnexpectedChar(']', "start of value", 7))
		, ("errors on empty value (array)", """[1,,3]""", mapBuilder, UnexpectedChar(',', "start of value", 3))
		, ("errors on empty value 2 (array)", """[,]""", mapBuilder, UnexpectedChar(',', "start of value", 1))
		, ("errors on trailing comma (object)", """{"a":2,}""", mapBuilder, UnexpectedChar('}', "start of object key", 7))
		, ("errors on empty value (object)", """{"":0,,}""", mapBuilder, UnexpectedChar(',', "start of object key", 6))
		, ("errors on empty value 2 (object)", """{,}""", mapBuilder, UnexpectedChar(',', "start of object key", 1))
		
		, ("provides a correct index in a nested value (array, array)", """[[,]]""", seq2Builder, UnexpectedChar(',', "start of value", 2))
		, ("provides a correct index in a nested value (array, object)", """[{},{,}]""", seq2Builder, UnexpectedChar(',', "start of object key", 5))
		, ("provides a correct index in a nested value (object, array)", """{"":[,]}""", seq2Builder, UnexpectedChar(',', "start of value", 5))
		, ("provides a correct index in a nested value (object, object)", """{"" : {,}}""", seq2Builder, UnexpectedChar(',', "start of object key", 7))
		
/*		, ("Throwbuilder (array of string)", """["ab"]""", throwBuilder, None, Option(1))
		, ("Throwbuilder (array of int -2)", """[-2]""", throwBuilder, None, Option(1))
		, ("Throwbuilder (array of int 5.5)", """[5.5]""", throwBuilder, None, Option(1))
		, ("Throwbuilder (array of true)", """[true]""", throwBuilder, None, Option(1))
		, ("Throwbuilder (object of string)", """{"":"ab"}""", throwBuilder, None, Option(4))
		, ("Throwbuilder (object of int -2)", """{"":-2}""", throwBuilder, None, Option(4))
		, ("Throwbuilder (object of int 5.5)", """{"":5.5}""", throwBuilder, None, Option(4))
		, ("Throwbuilder (object of true)", """{"":true}""", throwBuilder, None, Option(4))
*/	)
	
	
	describe("JsonParser") {
		failureCases.foreach{abcde =>
			val (name, source, builder, expected) = abcde
			
			it (name) {
				assertResult(ParserRetVal.ParserFailure(expected)){parser.parse(builder, source)}
			}
		}
		
		it ("""errors on control character inside string""") {
			('\u0000' until ' ').foreach{c =>
				val source = Seq('[', '"', c, '"', ']')
				
				assertResult(ParserRetVal.ParserFailure(ControlCharInString(c, 2))){
					parser.parse(mapBuilder, source)
				}
			}
		}
		it ("errors on infinitely nested arrays") {
			val source = new java.io.Reader {
				override def close():Unit = {}
				override def read():Int = '['
				override def read(chars:Array[Char], offset:Int, length:Int):Int = {
					(offset until (offset + length)).foreach{idx =>
						chars(idx) = '['
					}
					length
				}
			}
			
			assertResult(ParserRetVal.ParserFailure(TooDeeplyNested)){parser.parse(mapBuilder, source)}
		}
	}
}
