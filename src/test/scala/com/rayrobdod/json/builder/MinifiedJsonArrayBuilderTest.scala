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
import org.scalatest.FunSpec;
import java.nio.charset.StandardCharsets.US_ASCII;
import com.rayrobdod.json.union.{StringOrInt, JsonValue, CborValue}
import com.rayrobdod.json.union.JsonValue._
import com.rayrobdod.json.parser.{IdentityParser, FailureParser}
import com.rayrobdod.json.parser.{byteArray2DataInput, HexArrayStringConverter};

@deprecated("MinifiedJsonArrayBuilder is deprecated; using to suppress warnings tests related to that class", "3.0")
class MinifiedJsonArrayBuilderTest extends FunSpec {
	
	describe("MinifiedJsonArrayBuilder") {
		it ("inits correctly") {
			assertResult("[]"){new MinifiedJsonArrayBuilder().init}
		}
		it ("Appends null") {
			assertResult(Right("""[null]""")){
				new MinifiedJsonArrayBuilder().apply("[]", "",JsonValueNull, new IdentityParser[JsonValue])
			}
		}
		it ("Appends true") {
			assertResult(Right("""[true]""")){
				new MinifiedJsonArrayBuilder().apply("[]", "",JsonValue(true), new IdentityParser[JsonValue])
			}
		}
		it ("Appends false") {
			assertResult(Right("""[false]""")){
				new MinifiedJsonArrayBuilder().apply("[]", "",JsonValue(false), new IdentityParser[JsonValue])
			}
		}
		it ("Appends integer") {
			assertResult(Right("""[68]""")){
				new MinifiedJsonArrayBuilder().apply("[]", "",JsonValue(68), new IdentityParser[JsonValue])
			}
		}
		it ("Appends string") {
			assertResult(Right("""["abc"]""")){
				new MinifiedJsonArrayBuilder().apply("[]", "",JsonValue("abc"), new IdentityParser[JsonValue])
			}
		}
		it ("Appends string with escapes") {
			assertResult(Right("""["a\tc"]""")){
				new MinifiedJsonArrayBuilder().apply("[]", "",JsonValue("a\tc"), new IdentityParser[JsonValue])
			}
		}
		it ("Appends string with escapes 2") {
			assertResult(Right("""["a\""" + """u0000c"]""")){
				new MinifiedJsonArrayBuilder().apply("[]", "",JsonValue("a\u0000c"), new IdentityParser[JsonValue])
			}
		}
		it ("Appends string with space") {
			assertResult(Right("""[" a c "]""")){
				new MinifiedJsonArrayBuilder().apply("[]", "",JsonValue(" a c "), new IdentityParser[JsonValue])
			}
		}
		it ("Appends string with non-ascii char (utf-8)") {
			assertResult(Right("""["Pokémon"]""")){
				new MinifiedJsonArrayBuilder().apply("[]", "",JsonValue("Pokémon"), new IdentityParser[JsonValue])
			}
		}
		it ("Appends string with non-ascii char (ascii)") {
			assertResult(Right("""["Pok\""" + """u00e9mon"]""")){
				new MinifiedJsonArrayBuilder(US_ASCII).apply("[]", "",JsonValue("Pokémon"), new IdentityParser[JsonValue])
			}
		}
		it ("Appends a second value") {
			assertResult(Right("""["a","b","c","d"]""")){
				new MinifiedJsonArrayBuilder(US_ASCII).apply("""["a","b","c"]""", 3, JsonValue("d"), new IdentityParser[JsonValue])
			}
		}
		
		it ("When parser reports a failure, the failure is forwarded") {
			assertResult( Left("FailureParser", 0) ){
				new MinifiedJsonArrayBuilder().apply("[rest]", "", "", new FailureParser)
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
				).fold({x => x}, {x => x}, {(s,i) => ((s,i))})
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
				).fold({x => x}, {x => x}, {(s,i) => ((s,i))})
			}
		}
		it ("MinifiedJsonArrayBuilder + CborParser + primitives") {
			assertResult("""[5]"""){
				new CborParser().parse(
					new MinifiedJsonArrayBuilder().mapKey[CborValue].mapValue[CborValue](cborValueHexencodeByteStr),
					byteArray2DataInput(hexArray"A10405")
				).fold({x => x}, {x => x}, {(s,i) => ((s,i))})
			}
		}
	}
}
