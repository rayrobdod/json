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

class JsonParserTest_Unhappy extends FunSpec {
	describe("JsonParser") {
		it ("""errors when object is incomplete""") {
			val source = """{"""
			assertFailureParse("",1){
				new JsonParser().parse(MapBuilder[StringOrInt, JsonValue], source)
			}
		}
		it ("""errors when array is incomplete""") {
			val source = """ ["""
			assertFailureParse("",2){
				new JsonParser().parse(MapBuilder[StringOrInt, JsonValue], source)
			}
		}
		it ("""errors when string starts with non-space characters""") {
			val source = """abc"""
			assertFailureParse("",0){
				new JsonParser().parse(MapBuilder[StringOrInt, JsonValue], source)
			}
		}
		it ("""errors when string ends with non-space characters""") {
			val source = """[]abc"""
			assertFailureParse("",2){
				new JsonParser().parse(MapBuilder[StringOrInt, JsonValue], source)
			}
		}
		it ("""errors when map key is not a string""") {
			val source = """{2:3}"""
			assertFailureParse("",1){
				new JsonParser().parse(MapBuilder[StringOrInt, JsonValue], source)
			}
		}
		it ("""errors when map key is more than just a string""") {
			val source = """{"fd" null}"""
			assertFailureParse("",6){
				new JsonParser().parse(MapBuilder[StringOrInt, JsonValue], source)
			}
		}
		it ("""errors when map value is more than just a string""") {
			val source = """{" ":"hello" "world"}"""
			assertFailureParse("",13){
				new JsonParser().parse(MapBuilder[StringOrInt, JsonValue], source)
			}
		}
		it ("""errors when array value is more than just a string""") {
			val source = """["hello" "world"]"""
			assertFailureParse("",9){
				new JsonParser().parse(MapBuilder[StringOrInt, JsonValue], source)
			}
		}
		it ("""errors when object value is not alphanumeric""") {
			val source = """{"fd":%%%}"""
			assertFailureParse("",6){
				new JsonParser().parse(MapBuilder[StringOrInt, JsonValue], source)
			}
		}
		it ("""errors when array value is not alphanumeric""") {
			val source = """[%%%]"""
			assertFailureParse("",1){
				new JsonParser().parse(MapBuilder[StringOrInt, JsonValue], source)
			}
		}
		it ("""errors when array value is not a keyword""") {
			val source = """[nothing]"""
			assertFailureParse("",8){
				new JsonParser().parse(MapBuilder[StringOrInt, JsonValue], source)
			}
		}
		it ("""errors when number starts with a decimal point (array)""") {
			val source = """[.5]"""
			assertFailureParse("",1){
				new JsonParser().parse(MapBuilder[StringOrInt, JsonValue], source)
			}
		}
		it ("""errors when number starts with a decimal point (object)""") {
			val source = """{"":.5}"""
			assertFailureParse("",4){
				new JsonParser().parse(MapBuilder[StringOrInt, JsonValue], source)
			}
		}
		it ("""errors when number starts with an exponent indicator (array)""") {
			val source = """[e5]"""
			assertFailureParse("",3){
				new JsonParser().parse(MapBuilder[StringOrInt, JsonValue], source)
			}
		}
		it ("""errors when number starts with a exponent indicator (object)""") {
			val source = """{"":e5}"""
			assertFailureParse("",6){
				new JsonParser().parse(MapBuilder[StringOrInt, JsonValue], source)
			}
		}
		it ("""errors when number starts with a plus sign (array)""") {
			val source = """[+5]"""
			assertFailureParse("",1){
				new JsonParser().parse(MapBuilder[StringOrInt, JsonValue], source)
			}
		}
		it ("""errors when number starts with a plus sign (object)""") {
			val source = """{"":+5}"""
			assertFailureParse("",4){
				new JsonParser().parse(MapBuilder[StringOrInt, JsonValue], source)
			}
		}
		it ("""number format""") {
			val source = """{"":51sfd}"""
			assertFailureParse("",9){
				new JsonParser().parse(MapBuilder[StringOrInt, JsonValue], source)
			}
		}
		
		
		it ("""errors on control character inside string""") {
			('\u0000' until ' ').foreach{c =>
				val source = Seq('[', '"', c, '"', ']') 
				assertFailureParse("",2){
					new JsonParser().parse(MapBuilder[StringOrInt, JsonValue], source)
				}
			}
		}
		it ("""errors on illegal escape character inside string""") {
			val source = """["\a"]"""
			assertFailureParse("",3){
				new JsonParser().parse(MapBuilder[StringOrInt, JsonValue], source)
			}
		}
		it ("""errors on illegal character in unicode escape""") {
			val source = "[\"\\u1y34\"]"
			assertFailureParse("",5){
				new JsonParser().parse(MapBuilder[StringOrInt, JsonValue], source)
			}
		}
		it ("""errors on illegal character in unicode escape 2""") {
			val source = "[\"\\u1Y4\"]"
			assertFailureParse("",5){
				new JsonParser().parse(MapBuilder[StringOrInt, JsonValue], source)
			}
		}
		it ("""errors on illegal character in unicode escape 3""") {
			val source = "[\"\\u1 4\"]"
			assertFailureParse("",5){
				new JsonParser().parse(MapBuilder[StringOrInt, JsonValue], source)
			}
		}
		it ("""errors on illegal character in unicode escape 4""") {
			val source = "[\"\\u1=4\"]"
			assertFailureParse("",5){
				new JsonParser().parse(MapBuilder[StringOrInt, JsonValue], source)
			}
		}
		it ("""errors on trailing comma (array)""") {
			val source = """[1,2,3,]"""
			assertFailureParse("",7){
				new JsonParser().parse(MapBuilder[StringOrInt, JsonValue], source)
			}
		}
		it ("""errors on empty value (array)""") {
			val source = """[1,,3]"""
			assertFailureParse("",3){
				new JsonParser().parse(MapBuilder[StringOrInt, JsonValue], source)
			}
		}
		it ("""errors on empty value 2 (array)""") {
			val source = """[,]"""
			assertFailureParse("",1){
				new JsonParser().parse(MapBuilder[StringOrInt, JsonValue], source)
			}
		}
		it ("""errors on trailing comma (object)""") {
			val source = """{"a":2,}"""
			assertFailureParse("",7){
				new JsonParser().parse(MapBuilder[StringOrInt, JsonValue], source)
			}
		}
		it ("""errors on empty value (object)""") {
			val source = """{"":0,,}"""
			assertFailureParse("",6){
				new JsonParser().parse(MapBuilder[StringOrInt, JsonValue], source)
			}
		}
		it ("""errors on empty value 2 (object)""") {
			val source = """{,}"""
			assertFailureParse("",1){
				new JsonParser().parse(MapBuilder[StringOrInt, JsonValue], source)
			}
		}
		it ("""provides a correct index in a nested value (array, array)""") {
			val source = """[[,]]"""
			assertFailureParse("",2){
				new JsonParser().parse(new SeqBuilder(new PrimitiveSeqBuilder[JsonValue]), source)
			}
		}
		it ("""provides a correct index in a nested value (array, object)""") {
			val source = """[{},{,}]"""
			assertFailureParse("",5){
				new JsonParser().parse(new SeqBuilder(new PrimitiveSeqBuilder[JsonValue]), source)
			}
		}
		it ("""provides a correct index in a nested value (object, array)""") {
			val source = """{"":[,]}"""
			assertFailureParse("",5){
				new JsonParser().parse(new SeqBuilder(new PrimitiveSeqBuilder[JsonValue]), source)
			}
		}
		it ("""provides a correct index in a nested value (object, object)""") {
			val source = """{"" : {,}}"""
			assertFailureParse("",7){
				new JsonParser().parse(new SeqBuilder(new PrimitiveSeqBuilder[JsonValue]), source)
			}
		}
		it ("""Throwbuilder (array of string)""") {
			val source = """["ab"]"""
			assertFailureParse("",1){
				new JsonParser().parse(new ThrowBuilder[StringOrInt, JsonValue], source)
			}
		}
		it ("""Throwbuilder (array of int -2)""") {
			val source = """[-2]"""
			assertFailureParse("",1){
				new JsonParser().parse(new ThrowBuilder[StringOrInt, JsonValue], source)
			}
		}
		it ("""Throwbuilder (array of int 5.5)""") {
			val source = """[5.5]"""
			assertFailureParse("",1){
				new JsonParser().parse(new ThrowBuilder[StringOrInt, JsonValue], source)
			}
		}
		it ("""Throwbuilder (array of true)""") {
			val source = """[true]"""
			assertFailureParse("",1){
				new JsonParser().parse(new ThrowBuilder[StringOrInt, JsonValue], source)
			}
		}
		it ("""Throwbuilder (object of string)""") {
			val source = """{"":"ab"}"""
			assertFailureParse("",4){
				new JsonParser().parse(new ThrowBuilder[StringOrInt, JsonValue], source)
			}
		}
		it ("""Throwbuilder (object of int -2)""") {
			val source = """{"":-2}"""
			assertFailureParse("",4){
				new JsonParser().parse(new ThrowBuilder[StringOrInt, JsonValue], source)
			}
		}
		it ("""Throwbuilder (object of int 5.5)""") {
			val source = """{"":5.5}"""
			assertFailureParse("",4){
				new JsonParser().parse(new ThrowBuilder[StringOrInt, JsonValue], source)
			}
		}
		it ("""Throwbuilder (object of true)""") {
			val source = """{"":true}"""
			assertFailureParse("",4){
				new JsonParser().parse(new ThrowBuilder[StringOrInt, JsonValue], source)
			}
		}
	}
	
	def assertFailureParse(msg:String, idx:Int)(result:ParserRetVal[_,_]):Unit = result match {
		case ParserRetVal.Failure(msg2, idx2) => {
	//		assertResult(msg){msg2}
			assertResult(idx){idx2}
		}
		case x => fail("Not a Failure: " + x)
	}
}
