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
import com.rayrobdod.json.union.{StringOrInt, JsonValue}
import com.rayrobdod.json.union.JsonValue._
import com.rayrobdod.json.parser.IdentityParser
import com.rayrobdod.json.parser.{byteArray2DataInput, HexArrayStringConverter};

class MinifiedJsonArrayBuilderTest extends FunSpec {
	
	describe("MinifiedJsonArrayBuilder") {
		it ("inits correctly") {
			assertResult("[]"){new MinifiedJsonArrayBuilder().init}
		}
		it ("Appends null") {
			assertResult(Success("""[null]""")){
				new MinifiedJsonArrayBuilder().apply("", "[]", JsonValueNull, new IdentityParser[Any,JsonValue])
			}
		}
		it ("Appends true") {
			assertResult(Success("""[true]""")){
				new MinifiedJsonArrayBuilder().apply("", "[]", JsonValue(true), new IdentityParser[Any,JsonValue])
			}
		}
		it ("Appends false") {
			assertResult(Success("""[false]""")){
				new MinifiedJsonArrayBuilder().apply("", "[]", JsonValue(false), new IdentityParser[Any,JsonValue])
			}
		}
		it ("Appends integer") {
			assertResult(Success("""[68]""")){
				new MinifiedJsonArrayBuilder().apply("", "[]", JsonValue(68), new IdentityParser[Any,JsonValue])
			}
		}
		it ("Appends string") {
			assertResult(Success("""["abc"]""")){
				new MinifiedJsonArrayBuilder().apply("", "[]", JsonValue("abc"), new IdentityParser[Any,JsonValue])
			}
		}
		it ("Appends string with escapes") {
			assertResult(Success("""["a\tc"]""")){
				new MinifiedJsonArrayBuilder().apply("", "[]", JsonValue("a\tc"), new IdentityParser[Any,JsonValue])
			}
		}
		it ("Appends string with escapes 2") {
			assertResult(Success("""["a\""" + """u0000c"]""")){
				new MinifiedJsonArrayBuilder().apply("", "[]", JsonValue("a\u0000c"), new IdentityParser[Any,JsonValue])
			}
		}
		it ("Appends string with space") {
			assertResult(Success("""[" a c "]""")){
				new MinifiedJsonArrayBuilder().apply("", "[]", JsonValue(" a c "), new IdentityParser[Any,JsonValue])
			}
		}
		it ("Appends string with non-ascii char (utf-8)") {
			assertResult(Success("""["Pokémon"]""")){
				new MinifiedJsonArrayBuilder().apply("", "[]", JsonValue("Pokémon"), new IdentityParser[Any,JsonValue])
			}
		}
		it ("Appends string with non-ascii char (ascii)") {
			assertResult(Success("""["Pok\""" + """u00e9mon"]""")){
				new MinifiedJsonArrayBuilder(US_ASCII).apply("", "[]", JsonValue("Pokémon"), new IdentityParser[Any,JsonValue])
			}
		}
		it ("Appends a second value") {
			assertResult(Success("""["a","b","c","d"]""")){
				new MinifiedJsonArrayBuilder(US_ASCII).apply(3, """["a","b","c"]""", JsonValue("d"), new IdentityParser[Any,JsonValue])
			}
		}
	}
	
	describe("MinifiedJsonArrayBuilder integration") {
		import com.rayrobdod.json.parser.{JsonParser, CborParser}
		
		it ("MinifiedJsonArrayBuilder + JsonParser + primitive") {
			assertResult("""[61,62,63]"""){
				new JsonParser().parse(
					new MinifiedJsonArrayBuilder().mapKey[StringOrInt].mapValue[JsonValue],
					"""[61,62,63]"""
				).get.left.get
			}
		}
		it ("MinifiedJsonArrayBuilder + JsonParser + primitive (whitespace)") {
			assertResult("""["a","b","c"]"""){
				new JsonParser().parse(
					new MinifiedJsonArrayBuilder().mapKey[StringOrInt].mapValue[JsonValue],
					"""[
	"a",
	"b",
	"c"
]"""
				).get.left.get
			}
		}
		it ("MinifiedJsonArrayBuilder + CborParser + primitives") {
			assertResult("""[5]"""){
				new CborParser().parse(
					new MinifiedJsonArrayBuilder().mapKey[JsonValue].mapValue[JsonValue],
					byteArray2DataInput(hexArray"A10405")
				).get.left.get
			}
		}
	}
}
