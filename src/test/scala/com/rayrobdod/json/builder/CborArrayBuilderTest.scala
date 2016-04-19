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
import scala.collection.immutable.{Seq, Map};
import org.scalatest.FunSpec;
import java.nio.charset.StandardCharsets.US_ASCII;
import com.rayrobdod.json.union.JsonValue
import com.rayrobdod.json.union.StringOrInt
import com.rayrobdod.json.union.JsonValue._
import com.rayrobdod.json.union.JsonValueOrCollection
import com.rayrobdod.json.parser.IdentityParser
import com.rayrobdod.json.parser.HexArrayStringConverter;

class CborArrayBuilderTest extends FunSpec {
	private case class Abc(a:Int, b:Boolean, c:String)
	private implicit def classAbc = classOf[Abc]
	
	
	describe("CborArrayBuilder") {
		it ("inits correctly") {
			assertResult(Seq(0x80.byteValue)){new CborArrayBuilder().init}
		}
		it ("Appends null") {
			assertResult(hexSeq"81f6"){
				new CborArrayBuilder().apply("").apply(hexSeq"80", JsonValueOrCollection(JsonValueNull), new IdentityParser[Any, JsonValueOrCollection])
			}
		}
		it ("Appends true") {
			assertResult(hexSeq"81f5"){
				new CborArrayBuilder().apply("").apply(hexSeq"80", JsonValueOrCollection(true), new IdentityParser[Any, JsonValueOrCollection])
			}
		}
		it ("Appends false") {
			assertResult(hexSeq"81f4"){
				new CborArrayBuilder().apply("").apply(hexSeq"80", JsonValueOrCollection(false), new IdentityParser[Any, JsonValueOrCollection])
			}
		}
		it ("Appends 0 as one byte") {
			assertResult(hexSeq"8100"){
				new CborArrayBuilder().apply("").apply(hexSeq"80", JsonValueOrCollection(0), new IdentityParser[Any, JsonValueOrCollection])
			}
		}
		it ("Appends small positive integer") {
			assertResult(hexSeq"8106"){
				new CborArrayBuilder().apply("").apply(hexSeq"80", JsonValueOrCollection(6), new IdentityParser[Any, JsonValueOrCollection])
			}
		}
		it ("Appends 23 as one byte") {
			assertResult(hexSeq"8117"){
				new CborArrayBuilder().apply("").apply(hexSeq"80", JsonValueOrCollection(23), new IdentityParser[Any, JsonValueOrCollection])
			}
		}
		it ("Appends 24 as two bytes") {
			assertResult(hexSeq"811818"){
				new CborArrayBuilder().apply("").apply(hexSeq"80", JsonValueOrCollection(24), new IdentityParser[Any, JsonValueOrCollection])
			}
		}
		it ("Appends 255 as two bytes") {
			assertResult(hexSeq"8118FF"){
				new CborArrayBuilder().apply("").apply(hexSeq"80", JsonValueOrCollection(255), new IdentityParser[Any, JsonValueOrCollection])
			}
		}
		it ("Appends 256 as three bytes") {
			assertResult(hexSeq"81190100"){
				new CborArrayBuilder().apply("").apply(hexSeq"80", JsonValueOrCollection(256), new IdentityParser[Any, JsonValueOrCollection])
			}
		}
		it ("Appends -1 as one byte") {
			assertResult(hexSeq"8120"){
				new CborArrayBuilder().apply("").apply(hexSeq"80", JsonValueOrCollection(-1), new IdentityParser[Any, JsonValueOrCollection])
			}
		}
		it ("Appends -100 as two bytes") {
			assertResult(hexSeq"813863"){
				new CborArrayBuilder().apply("").apply(hexSeq"80", JsonValueOrCollection(-100), new IdentityParser[Any, JsonValueOrCollection])
			}
		}
		it ("Appends Float") {
			assertResult(hexArray"81 FA3FC00000"){
				new CborArrayBuilder().apply("").apply(hexSeq"80", JsonValueOrCollection(1.5f), new IdentityParser[Any, JsonValueOrCollection])
			}
		}
		it ("Appends Double") {
			assertResult(hexArray"81 fbc010666666666666"){
				new CborArrayBuilder().apply("").apply(hexSeq"80", JsonValueOrCollection(-4.1D), new IdentityParser[Any, JsonValueOrCollection])
			}
		}
		it ("Appends empty string") {
			assertResult(hexSeq"8160"){
				new CborArrayBuilder().apply("").apply(hexSeq"80", JsonValueOrCollection(""), new IdentityParser[Any, JsonValueOrCollection])
			}
		}
		it ("Appends \"abc\"") {
			assertResult(hexSeq"8163616263"){
				new CborArrayBuilder().apply("").apply(hexSeq"80", JsonValueOrCollection("abc"), new IdentityParser[Any, JsonValueOrCollection])
			}
		}
		it ("Appends string containing multibyte char") {
			assertResult(hexSeq"8163e6b0b4"){
				new CborArrayBuilder().apply("").apply(hexSeq"80", JsonValueOrCollection("\u6c34"), new IdentityParser[Any, JsonValueOrCollection])
			}
		}
		it ("Appends an empty byte array") {
			assertResult(hexSeq"8140"){
				new CborArrayBuilder().apply("").apply(hexSeq"80", JsonValueOrCollection(new Array[Byte](0)), new IdentityParser[Any, JsonValueOrCollection])
			}
		}
		it ("Appends a non-empty byte array") {
			assertResult(hexSeq"8143010203"){
				new CborArrayBuilder().apply("").apply(hexSeq"80", JsonValueOrCollection(hexArray"010203"), new IdentityParser[Any, JsonValueOrCollection])
			}
		}
		it ("Appends a second value") {
			assertResult(hexSeq"""83616161626164"""){
				new CborArrayBuilder().apply("c").apply(hexSeq"""8261616162""", JsonValueOrCollection("d"), new IdentityParser[Any, JsonValueOrCollection])
			}
		}
		it ("Appends a 24th value") {
			assertResult(hexSeq"""9818 B7A8B7A8B7A8 60"""){
				new CborArrayBuilder().apply("").apply(hexSeq"""97 B7A8B7A8B7A8 """, JsonValueOrCollection(""), new IdentityParser[Any, JsonValueOrCollection])
			}
		}
		it ("Appends a 101th value") {
			assertResult(hexSeq"""9903e9 B7A8B7A8B7A8 60"""){
				new CborArrayBuilder().apply("").apply(hexSeq"""9903e8 B7A8B7A8B7A8 """, JsonValueOrCollection(""), new IdentityParser[Any, JsonValueOrCollection])
			}
		}
		it ("Appends a 256th value") {
			assertResult(hexSeq"""990100 B7A8B7A8B7A8 60"""){
				new CborArrayBuilder().apply("").apply(hexSeq"""98FF B7A8B7A8B7A8 """, JsonValueOrCollection(""), new IdentityParser[Any, JsonValueOrCollection])
			}
		}
		it ("Appends a 65536th value") {
			assertResult(hexSeq"""9A00010000 B7A8B7A8B7A8 60"""){
				new CborArrayBuilder().apply("").apply(hexSeq"""99FFFF B7A8B7A8B7A8 """, JsonValueOrCollection(""), new IdentityParser[Any, JsonValueOrCollection])
			}
		}
		it ("Appends a 1000001th value") {
			assertResult(hexSeq"""9a000f4241 B7A8B7A8B7A8 60"""){
				new CborArrayBuilder().apply("").apply(hexSeq"""9a000f4240 B7A8B7A8B7A8 """, JsonValueOrCollection(""), new IdentityParser[Any, JsonValueOrCollection])
			}
		}
	}
	
	describe("CborArrayBuilder integration") {
		import com.rayrobdod.json.parser.{JsonParser, SeqParser, CborParser, CaseClassParser, byteArray2DataInput}
		
		it ("CborArrayBuilder + JsonParser + primitive") {
			assertResult(hexSeq"83 183D 183E 183F"){
				new JsonParser().parseComplex(
					new CborArrayBuilder().mapKey[StringOrInt].mapValue[JsonValue],
					"""[61, 62, 63]"""
				)
			}
		}
		ignore ("CborArrayBuilder + JsonParser + nested objects") {
			assertResult(hexSeq"81 A2 616100 616201"){
				new JsonParser().parseComplex(
					new CborArrayBuilder().mapKey[StringOrInt].mapValue[JsonValue],
					"""[{"a":0,"b":1}]"""
				)
			}
		}
		it ("CborArrayBuilder + CborParser + primitives") {
			assertResult(hexSeq"8262202005"){
				new CborParser().parseComplex(
					new CborArrayBuilder().mapKey[JsonValue].mapValue[JsonValue],
					byteArray2DataInput(
							hexArray"8262202005"
					)
				)
			}
		}
		ignore ("CborArrayBuilder + SeqParser + nested arrays") {
			assertResult(hexSeq"81818105"){
				new SeqParser().parseComplex(
					new CborArrayBuilder().mapKey[Int].mapValue[JsonValue].mapValue[Object]{JsonValue.unsafeWrap _},
					Seq(Seq(Seq(5)))
				)
			}
		}
		ignore ("CborArrayBuilder + nested case classes") {
			assertResult(hexSeq"81 A3 6161 05 6162 f4 6163 63737472"){
				new SeqParser().parseComplex(
					new CborArrayBuilder().mapKey[Int].mapValue[Object]{JsonValue.unsafeWrap _},
					Seq(Abc(5,false,"str"))
				)
			}
		}
	}
}
