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
package com.rayrobdod.json.builder;

import scala.beans.BeanProperty;
import java.text.ParseException;
import scala.collection.immutable.Map;
import org.scalatest.FunSpec;
import java.nio.charset.StandardCharsets.US_ASCII;
import com.rayrobdod.json.parser.{byteArray2DataInput, HexArrayStringConverter};

class MinifiedJsonArrayBuilderTest extends FunSpec {
	
	describe("MinifiedJsonArrayBuilder") {
		it ("inits correctly") {
			assertResult("[]"){new MinifiedJsonArrayBuilder().init}
		}
		it ("Appends null") {
			assertResult("""[null]"""){
				new MinifiedJsonArrayBuilder().apply("[]", "", null)
			}
		}
		it ("Appends true") {
			assertResult("""[true]"""){
				new MinifiedJsonArrayBuilder().apply("[]", "", true)
			}
		}
		it ("Appends false") {
			assertResult("""[false]"""){
				new MinifiedJsonArrayBuilder().apply("[]", "", false)
			}
		}
		it ("Appends integer") {
			assertResult("""[68]"""){
				new MinifiedJsonArrayBuilder().apply("[]", "", 68)
			}
		}
		it ("Appends string") {
			assertResult("""["abc"]"""){
				new MinifiedJsonArrayBuilder().apply("[]", "", "abc")
			}
		}
		it ("Appends string with escapes") {
			assertResult("""["a\tc"]"""){
				new MinifiedJsonArrayBuilder().apply("[]", "", "a\tc")
			}
		}
		it ("Appends string with escapes 2") {
			assertResult("""["a\""" + """u0000c"]"""){
				new MinifiedJsonArrayBuilder().apply("[]", "", "a\u0000c")
			}
		}
		it ("Appends string with space") {
			assertResult("""[" a c "]"""){
				new MinifiedJsonArrayBuilder().apply("[]", "", " a c ")
			}
		}
		it ("Appends string with non-ascii char (utf-8)") {
			assertResult("""["Pokémon"]"""){
				new MinifiedJsonArrayBuilder().apply("[]", "", "Pokémon")
			}
		}
		it ("Appends string with non-ascii char (ascii)") {
			assertResult("""["Pok\""" + """u00e9mon"]"""){
				new MinifiedJsonArrayBuilder(US_ASCII).apply("[]", "", "Pokémon")
			}
		}
		it ("Appends a second value") {
			assertResult("""["a","b","c","d"]"""){
				new MinifiedJsonArrayBuilder().apply("""["a","b","c"]""", "3", "d")
			}
		}
		it ("childBuilder returns MapBuilder") {
			val builder = new MinifiedJsonArrayBuilder()
			
			assert{builder.childBuilder("").isInstanceOf[MapBuilder[_]]}
		}
		it ("resultType returns constructor parameter `clazz`") {
			assertResult(classOf[String]){
				new MinifiedJsonArrayBuilder().resultType
			}
		}
	}
	
	describe("MinifiedJsonArrayBuilder integration") {
		import com.rayrobdod.json.parser.{JsonParser, CborParser}
		
		it ("MinifiedJsonArrayBuilder + JsonParser + primitive") {
			assertResult("""[61,62,63]"""){
				new JsonParser(new MinifiedJsonArrayBuilder).parse(
					"""[61,62,63]"""
				)
			}
		}
		it ("MinifiedJsonArrayBuilder + JsonParser + primitive (whitespace)") {
			assertResult("""["a","b","c"]"""){
				new JsonParser(new MinifiedJsonArrayBuilder).parse(
					"""[
	"a",
	"b",
	"c"
]"""
				)
			}
		}
		ignore ("MinifiedJsonArrayBuilder + JsonParser + nested objects") {
			assertResult("""[{"a":0,"b":1}]"""){
				new JsonParser(new MinifiedJsonArrayBuilder).parse(
					"""[{"a":0,"b":1}]"""
				)
			}
		}
		it ("MinifiedJsonArrayBuilder + CborParser + primitives") {
			assertResult("""[5]"""){
				new CborParser(new MinifiedJsonArrayBuilder).parse(
					byteArray2DataInput(hexArray"A10405")
				)
			}
		}
	}
}
