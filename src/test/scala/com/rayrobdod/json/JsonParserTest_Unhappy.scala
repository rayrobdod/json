/*
	Copyright (c) 2015, Raymond Dodge
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

import java.text.ParseException;
import scala.collection.immutable.Map;
import org.scalatest.FunSpec;
import com.rayrobdod.json.builder.MapBuilder;

class JsonParserTest_Unhappy extends FunSpec {
	describe("JsonParser") {
		it ("""errors when object is incomplete""") {
			val source = """{"""
			val ex = intercept[ParseException]{
				new JsonParser(new MapBuilder()).parse(source)
			}
			assertResult(1){ex.getErrorOffset()}
		}
		it ("""errors when array is incomplete""") {
			val source = """ ["""
			val ex = intercept[ParseException]{
				new JsonParser(new MapBuilder()).parse(source)
			}
			assertResult(2){ex.getErrorOffset()}
		}
		it ("""errors when string starts with non-space characters""") {
			val source = """abc"""
			val ex = intercept[ParseException]{
				new JsonParser(new MapBuilder()).parse(source)
			}
			assertResult(0){ex.getErrorOffset()}
		}
		it ("""errors when string ends with non-space characters""") {
			val source = """[]abc"""
			val ex = intercept[ParseException]{
				new JsonParser(new MapBuilder()).parse(source)
			}
			assertResult(2){ex.getErrorOffset()}
		}
		it ("""errors when map key is not a string""") {
			val source = """{2:3}"""
			val ex = intercept[ParseException]{
				new JsonParser(new MapBuilder()).parse(source)
			}
			assertResult(1){ex.getErrorOffset()}
		}
		it ("""errors when map key is more than just a string""") {
			val source = """{"fd" null}"""
			val ex = intercept[ParseException]{
				new JsonParser(new MapBuilder()).parse(source)
			}
			assertResult(6){ex.getErrorOffset()}
		}
		it ("""errors when map value is more than just a string""") {
			val source = """{" ":"hello" "world"}"""
			val ex = intercept[ParseException]{
				new JsonParser(new MapBuilder()).parse(source)
			}
			assertResult(13){ex.getErrorOffset()}
		}
		it ("""errors when array value is more than just a string""") {
			val source = """["hello" "world"]"""
			val ex = intercept[ParseException]{
				new JsonParser(new MapBuilder()).parse(source)
			}
			assertResult(9){ex.getErrorOffset()}
		}
		it ("""errors when object value is not alphanumeric""") {
			val source = """{"fd":%%%}"""
			val ex = intercept[ParseException]{
				new JsonParser(new MapBuilder()).parse(source)
			}
			assertResult(6){ex.getErrorOffset()}
		}
		it ("""errors when array value is not alphanumeric""") {
			val source = """[%%%]"""
			val ex = intercept[ParseException]{
				new JsonParser(new MapBuilder()).parse(source)
			}
			assertResult(1){ex.getErrorOffset()}
		}
		it ("""errors when array value is not a keyword""") {
			val source = """[nothing]"""
			val ex = intercept[ParseException]{
				new JsonParser(new MapBuilder()).parse(source)
			}
			assertResult(8){ex.getErrorOffset()}
		}
		it ("""errors when number starts with a decimal point (array)""") {
			val source = """[.5]"""
			val ex = intercept[ParseException]{
				new JsonParser(new MapBuilder()).parse(source)
			}
			assertResult(1){ex.getErrorOffset()}
		}
		it ("""errors when number starts with a decimal point (object)""") {
			val source = """{"":.5}"""
			val ex = intercept[ParseException]{
				new JsonParser(new MapBuilder()).parse(source)
			}
			assertResult(4){ex.getErrorOffset()}
		}
		it ("""errors when number starts with an exponent indicator (array)""") {
			val source = """[e5]"""
			val ex = intercept[ParseException]{
				new JsonParser(new MapBuilder()).parse(source)
			}
			assertResult(3){ex.getErrorOffset()}
		}
		it ("""errors when number starts with a exponent indicator (object)""") {
			val source = """{"":e5}"""
			val ex = intercept[ParseException]{
				new JsonParser(new MapBuilder()).parse(source)
			}
			assertResult(6){ex.getErrorOffset()}
		}
		it ("""errors when number starts with a plus sign (array)""") {
			val source = """[+5]"""
			val ex = intercept[ParseException]{
				new JsonParser(new MapBuilder()).parse(source)
			}
			assertResult(1){ex.getErrorOffset()}
		}
		it ("""errors when number starts with a plus sign (object)""") {
			val source = """{"":+5}"""
			val ex = intercept[ParseException]{
				new JsonParser(new MapBuilder()).parse(source)
			}
			assertResult(4){ex.getErrorOffset()}
		}
		
		
		it ("""errors on control character inside string""") {
			('\0' until ' ').foreach{c =>
				val source = Seq('[', '"', c, '"', ']') 
				val ex = intercept[ParseException]{
					val result = new JsonParser(new MapBuilder()).parse(source)
				}
				assertResult(2){ex.getErrorOffset()}
			}
		}
		it ("""errors on illegal escape character inside string""") {
			val source = """["\a"]"""
			val ex =intercept[ParseException]{
				new JsonParser(new MapBuilder()).parse(source)
			}
			assertResult(3){ex.getErrorOffset()}
		}
		it ("""errors on illegal character in unicode escape""") {
			val source = "[\"\\u1y34\"]"
			val ex = intercept[ParseException]{
				new JsonParser(new MapBuilder()).parse(source)
			}
			assertResult(5){ex.getErrorOffset()}
		}
		it ("""errors on illegal character in unicode escape 2""") {
			val source = "[\"\\u1Y4\"]"
			val ex = intercept[ParseException]{
				new JsonParser(new MapBuilder()).parse(source)
			}
			assertResult(5){ex.getErrorOffset()}
		}
		it ("""errors on illegal character in unicode escape 3""") {
			val source = "[\"\\u1 4\"]"
			val ex = intercept[ParseException]{
				new JsonParser(new MapBuilder()).parse(source)
			}
			assertResult(5){ex.getErrorOffset()}
		}
		it ("""errors on illegal character in unicode escape 4""") {
			val source = "[\"\\u1=4\"]"
			val ex = intercept[ParseException]{
				new JsonParser(new MapBuilder()).parse(source)
			}
			assertResult(5){ex.getErrorOffset()}
		}
	}
}
