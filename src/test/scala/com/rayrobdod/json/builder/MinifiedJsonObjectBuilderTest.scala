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
package com.rayrobdod.json.builder;

import scala.beans.BeanProperty;
import java.text.ParseException;
import scala.collection.immutable.Map;
import scala.util.{Try, Success, Failure}
import org.scalatest.FunSpec;
import java.nio.charset.StandardCharsets.US_ASCII;
import com.rayrobdod.json.union.JsonValue
import com.rayrobdod.json.union.JsonValue._
import com.rayrobdod.json.parser.IdentityParser
import com.rayrobdod.json.parser.{byteArray2DataInput, HexArrayStringConverter}
import com.rayrobdod.json.union.StringOrInt

class MinifiedJsonObjectBuilderTest extends FunSpec {
	private case class Abc(a:Int, b:Boolean, c:String)
	private implicit def classAbc = classOf[Abc]
	
	describe("MinifiedJsonObjectBuilder") {
		it ("inits correctly") {
			assertResult("{}"){new MinifiedJsonObjectBuilder().init}
		}
		it ("Appends null") {
			assertResult(Success("""{"":null}""")){
				new MinifiedJsonObjectBuilder().apply("", "{}", JsonValueNull, new IdentityParser[String,JsonValue])
			}
		}
		it ("Appends true") {
			assertResult(Success("""{"":true}""")){
				new MinifiedJsonObjectBuilder().apply("", "{}", JsonValue(true), new IdentityParser[String,JsonValue])
			}
		}
		it ("Appends false") {
			assertResult(Success("""{"":false}""")){
				new MinifiedJsonObjectBuilder().apply("", "{}", JsonValue(false), new IdentityParser[String,JsonValue])
			}
		}
		it ("Appends integer") {
			assertResult(Success("""{"":68}""")){
				new MinifiedJsonObjectBuilder().apply("", "{}", JsonValue(68), new IdentityParser[String,JsonValue])
			}
		}
		it ("Appends string") {
			assertResult(Success("""{"":"abc"}""")){
				new MinifiedJsonObjectBuilder().apply("", "{}", JsonValue("abc"), new IdentityParser[String,JsonValue])
			}
		}
		it ("Appends string with escapes") {
			assertResult(Success("""{"":"a\tc"}""")){
				new MinifiedJsonObjectBuilder().apply("", "{}", JsonValue("a\tc"), new IdentityParser[String,JsonValue])
			}
		}
		it ("Appends string with escapes 2") {
			assertResult(Success("""{"":"a\""" + """u0000c"}""")){
				new MinifiedJsonObjectBuilder().apply("", "{}", JsonValue("a\u0000c"), new IdentityParser[String,JsonValue])
			}
		}
		it ("Appends string with space") {
			assertResult(Success("""{"":" a c "}""")){
				new MinifiedJsonObjectBuilder().apply("", "{}", JsonValue(" a c "), new IdentityParser[String,JsonValue])
			}
		}
		it ("Appends string with non-ascii char (utf-8)") {
			assertResult(Success("""{"":"Pokémon"}""")){
				new MinifiedJsonObjectBuilder().apply("", "{}", JsonValue("Pokémon"), new IdentityParser[String,JsonValue])
			}
		}
		it ("Appends string with non-ascii char (ascii)") {
			assertResult(Success("""{"":"Pok\""" + """u00e9mon"}""")){
				new MinifiedJsonObjectBuilder(US_ASCII).apply("", "{}", JsonValue("Pokémon"), new IdentityParser[String,JsonValue])
			}
		}
		it ("Appends a second value") {
			assertResult(Success("""{"a":"b","c":"d"}""")){
				new MinifiedJsonObjectBuilder().apply("c", """{"a":"b"}""", JsonValue("d"), new IdentityParser[String,JsonValue])
			}
		}
	}
	
	describe("MinifiedJsonObjectBuilder integration") {
		import com.rayrobdod.json.parser.{JsonParser, CborParser, CaseClassParser, MapParser}
		val builder = new MinifiedJsonObjectBuilder().mapKey[StringOrInt]{StringOrInt.unwrapToString}
		
		it ("MinifiedJsonObjectBuilder + JsonParser + primitive") {
			assertResult("""{"a":61,"b":62,"c":63}"""){
				new JsonParser().parse(
					builder,
					"""{"a":61,"b":62,"c":63}"""
				).get.left.get
			}
		}
		it ("MinifiedJsonObjectBuilder + JsonParser + primitive (whitespace)") {
			assertResult("""{"a":61,"b":62,"c":63}"""){
				new JsonParser().parse(
					builder,
					"""{
	"a" : 61,
	"b" : 62,
	"c" : 63
}"""
				).get.left.get
			}
		}
		it ("MinifiedJsonObjectBuilder + JsonParser + nested objects") {
			assertResult("""{"":{"a":0,"b":1}}"""){
				new JsonParser().parse(
					builder,
					"""{"":{"a":0,"b":1}}"""
				).get.left.get
			}
		}
		it ("MinifiedJsonObjectBuilder + CborParser + primitives") {
			assertResult("""{"4":5}"""){
				new CborParser().parse(
					new MinifiedJsonObjectBuilder().mapKey[JsonValue]{_ match {case JsonValueString(x) => x; case JsonValueNumber(x) => x.toString}},
					byteArray2DataInput(hexArray"A10405")
				).get.left.get
			}
		}
		it ("MinifiedJsonObjectBuilder + case class") {
			assertResult("""{"a":5,"b":false,"c":"str"}"""){
				new CaseClassParser().parse(
					new MinifiedJsonObjectBuilder().mapValue[Any]{JsonValue.unsafeWrap},
					Abc(5,false,"str")
				).get.fold({x => x}, {x => throw new IllegalArgumentException()})
			}
		}
		ignore ("MinifiedJsonObjectBuilder + nested case class") {
			assertResult("""{"5":{"a":5,"b":false,"c":"str"}}"""){
				new MapParser().parse(
					new MinifiedJsonObjectBuilder().mapValue[Any]{JsonValue.unsafeWrap},
					Map("5" -> Abc(5,false,"str"))
				).get.fold({x => x}, {x => throw new IllegalArgumentException()})
			}
		}
	}
}
