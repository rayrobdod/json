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

import org.scalatest.FunSpec;
import java.nio.charset.StandardCharsets.US_ASCII;
import com.rayrobdod.json.union.CborValue
import com.rayrobdod.json.union.JsonValue
import com.rayrobdod.json.union.JsonValue._
import com.rayrobdod.json.parser.{IdentityParser, FailureParser}
import com.rayrobdod.json.parser.{byteArray2DataInput, HexArrayStringConverter}
import com.rayrobdod.json.union.StringOrInt

@deprecated("MinifiedJsonObjectBuilder is deprecated; using to suppress warnings tests related to that class", "3.0")
class MinifiedJsonObjectBuilderTest extends FunSpec {
	private case class Abc(a:Int, b:Boolean, c:String)
	private implicit def classAbc = classOf[Abc]
	
	describe("MinifiedJsonObjectBuilder") {
		it ("inits correctly") {
			assertResult("{}"){new MinifiedJsonObjectBuilder().init}
		}
		it ("Appends null") {
			assertResult(Right("""{"":null}""")){
				new MinifiedJsonObjectBuilder().apply("{}", "",JsonValueNull, new IdentityParser[JsonValue])
			}
		}
		it ("Appends true") {
			assertResult(Right("""{"":true}""")){
				new MinifiedJsonObjectBuilder().apply("{}", "",JsonValue(true), new IdentityParser[JsonValue])
			}
		}
		it ("Appends false") {
			assertResult(Right("""{"":false}""")){
				new MinifiedJsonObjectBuilder().apply("{}", "",JsonValue(false), new IdentityParser[JsonValue])
			}
		}
		it ("Appends integer") {
			assertResult(Right("""{"":68}""")){
				new MinifiedJsonObjectBuilder().apply("{}", "",JsonValue(68), new IdentityParser[JsonValue])
			}
		}
		it ("Appends string") {
			assertResult(Right("""{"":"abc"}""")){
				new MinifiedJsonObjectBuilder().apply("{}", "",JsonValue("abc"), new IdentityParser[JsonValue])
			}
		}
		it ("Appends string with escapes") {
			assertResult(Right("""{"":"a\tc"}""")){
				new MinifiedJsonObjectBuilder().apply("{}", "",JsonValue("a\tc"), new IdentityParser[JsonValue])
			}
		}
		it ("Appends string with escapes 2") {
			assertResult(Right("""{"":"a\""" + """u0000c"}""")){
				new MinifiedJsonObjectBuilder().apply("{}", "",JsonValue("a\u0000c"), new IdentityParser[JsonValue])
			}
		}
		it ("Appends string with space") {
			assertResult(Right("""{"":" a c "}""")){
				new MinifiedJsonObjectBuilder().apply("{}", "",JsonValue(" a c "), new IdentityParser[JsonValue])
			}
		}
		it ("Appends string with non-ascii char (utf-8)") {
			assertResult(Right("""{"":"Pokémon"}""")){
				new MinifiedJsonObjectBuilder().apply("{}", "",JsonValue("Pokémon"), new IdentityParser[JsonValue])
			}
		}
		it ("Appends string with non-ascii char (ascii)") {
			assertResult(Right("""{"":"Pok\""" + """u00e9mon"}""")){
				new MinifiedJsonObjectBuilder(US_ASCII).apply("{}", "",JsonValue("Pokémon"), new IdentityParser[JsonValue])
			}
		}
		it ("Appends a second value") {
			assertResult(Right("""{"a":"b","c":"d"}""")){
				new MinifiedJsonObjectBuilder().apply("""{"a":"b"}""", "c", JsonValue("d"), new IdentityParser[JsonValue])
			}
		}
		
		it ("When parser reports a failure, the failure is forwarded") {
			assertResult( Left("FailureParser", 0) ){
				new MinifiedJsonObjectBuilder().apply("{rest}", "", "", new FailureParser)
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
				).fold({x => x}, {x => x}, {(s,i) => ((s,i))})
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
				).fold({x => x}, {x => x}, {(s,i) => ((s,i))})
			}
		}
		it ("MinifiedJsonObjectBuilder + JsonParser + nested objects") {
			assertResult("""{"":{"a":0,"b":1}}"""){
				new JsonParser().parse(
					builder,
					"""{"":{"a":0,"b":1}}"""
				).fold({x => x}, {x => x}, {(s,i) => ((s,i))})
			}
		}
		it ("MinifiedJsonObjectBuilder + CborParser + primitives") {
			assertResult("""{"4":5}"""){
				new CborParser().parse(
					new MinifiedJsonObjectBuilder()
							.mapKey[CborValue]{_ match {case CborValue.CborValueNumber(x) => x.toString; case _ => ""}}
							.mapValue[CborValue]{_ match {case CborValue.CborValueNumber(x) => x; case _ => JsonValueNull}},
					byteArray2DataInput(hexArray"A10405")
				).fold({x => x}, {x => x}, {(s,i) => ((s,i))})
			}
		}
		it ("MinifiedJsonObjectBuilder + case class") {
			assertResult("""{"a":5,"b":false,"c":"str"}"""){
				new CaseClassParser().parse(
					new MinifiedJsonObjectBuilder().mapValue[Any]{JsonValue.unsafeWrap},
					Abc(5,false,"str")
				).fold({x => x}, {x => x}, {(s,i) => ((s,i))})
			}
		}
		ignore ("MinifiedJsonObjectBuilder + nested case class") {
			assertResult("""{"5":{"a":5,"b":false,"c":"str"}}"""){
				new MapParser().parse(
					new MinifiedJsonObjectBuilder().mapValue[Any]{JsonValue.unsafeWrap},
					Map("5" -> Abc(5,false,"str"))
				).fold({x => x}, {x => x}, {(s,i) => ((s,i))})
			}
		}
	}
}
