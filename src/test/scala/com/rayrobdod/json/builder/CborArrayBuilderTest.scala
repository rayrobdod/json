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
import scala.util.{Try, Success, Failure}
import scala.collection.immutable.{Seq, Map};
import org.scalatest.FunSpec;
import java.nio.charset.StandardCharsets.US_ASCII;
import com.rayrobdod.json.union.JsonValue
import com.rayrobdod.json.union.StringOrInt
import com.rayrobdod.json.union.JsonValue._
import com.rayrobdod.json.union.JsonValue
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
			assertResult(Success(hexSeq"81f6")){
				new CborArrayBuilder().apply("", hexSeq"80", JsonValueNull, new IdentityParser[Any, JsonValue])
			}
		}
		it ("Appends true") {
			assertResult(Success(hexSeq"81f5")){
				new CborArrayBuilder().apply("", hexSeq"80", JsonValue(true), new IdentityParser[Any, JsonValue])
			}
		}
		it ("Appends false") {
			assertResult(Success(hexSeq"81f4")){
				new CborArrayBuilder().apply("", hexSeq"80", JsonValue(false), new IdentityParser[Any, JsonValue])
			}
		}
		it ("Appends 0 as one byte") {
			assertResult(Success(hexSeq"8100")){
				new CborArrayBuilder().apply("", hexSeq"80", JsonValue(0), new IdentityParser[Any, JsonValue])
			}
		}
		it ("Appends small positive integer") {
			assertResult(Success(hexSeq"8106")){
				new CborArrayBuilder().apply("", hexSeq"80", JsonValue(6), new IdentityParser[Any, JsonValue])
			}
		}
		it ("Appends 23 as one byte") {
			assertResult(Success(hexSeq"8117")){
				new CborArrayBuilder().apply("", hexSeq"80", JsonValue(23), new IdentityParser[Any, JsonValue])
			}
		}
		it ("Appends 24 as two bytes") {
			assertResult(Success(hexSeq"811818")){
				new CborArrayBuilder().apply("", hexSeq"80", JsonValue(24), new IdentityParser[Any, JsonValue])
			}
		}
		it ("Appends 255 as two bytes") {
			assertResult(Success(hexSeq"8118FF")){
				new CborArrayBuilder().apply("", hexSeq"80", JsonValue(255), new IdentityParser[Any, JsonValue])
			}
		}
		it ("Appends 256 as three bytes") {
			assertResult(Success(hexSeq"81190100")){
				new CborArrayBuilder().apply("", hexSeq"80", JsonValue(256), new IdentityParser[Any, JsonValue])
			}
		}
		it ("Appends -1 as one byte") {
			assertResult(Success(hexSeq"8120")){
				new CborArrayBuilder().apply("", hexSeq"80", JsonValue(-1), new IdentityParser[Any, JsonValue])
			}
		}
		it ("Appends -100 as two bytes") {
			assertResult(Success(hexSeq"813863")){
				new CborArrayBuilder().apply("", hexSeq"80", JsonValue(-100), new IdentityParser[Any, JsonValue])
			}
		}
		it ("Appends Float") {
			assertResult(Success(hexSeq"81 FA3FC00000")){
				new CborArrayBuilder().apply("", hexSeq"80", JsonValue(1.5f), new IdentityParser[Any, JsonValue])
			}
		}
		it ("Appends Double") {
			assertResult(Success(hexSeq"81 fbc010666666666666")){
				new CborArrayBuilder().apply("", hexSeq"80", JsonValue(-4.1D), new IdentityParser[Any, JsonValue])
			}
		}
		it ("Appends empty string") {
			assertResult(Success(hexSeq"8160")){
				new CborArrayBuilder().apply("", hexSeq"80", JsonValue(""), new IdentityParser[Any, JsonValue])
			}
		}
		it ("Appends \"abc\"") {
			assertResult(Success(hexSeq"8163616263")){
				new CborArrayBuilder().apply("", hexSeq"80", JsonValue("abc"), new IdentityParser[Any, JsonValue])
			}
		}
		it ("Appends string containing multibyte char") {
			assertResult(Success(hexSeq"8163e6b0b4")){
				new CborArrayBuilder().apply("", hexSeq"80", JsonValue("\u6c34"), new IdentityParser[Any, JsonValue])
			}
		}
		it ("Appends an empty byte array") {
			assertResult(Success(hexSeq"8140")){
				new CborArrayBuilder().apply("", hexSeq"80", JsonValue(new Array[Byte](0)), new IdentityParser[Any, JsonValue])
			}
		}
		it ("Appends a non-empty byte array") {
			assertResult(Success(hexSeq"8143010203")){
				new CborArrayBuilder().apply("", hexSeq"80", JsonValue(hexArray"010203"), new IdentityParser[Any, JsonValue])
			}
		}
		it ("Appends a second value") {
			assertResult(Success(hexSeq"""83616161626164""")){
				new CborArrayBuilder().apply("c", hexSeq"""8261616162""", JsonValue("d"), new IdentityParser[Any, JsonValue])
			}
		}
		it ("Appends a 24th value") {
			assertResult(Success(hexSeq"""9818 B7A8B7A8B7A8 60""")){
				new CborArrayBuilder().apply("", hexSeq"""97 B7A8B7A8B7A8 """, JsonValue(""), new IdentityParser[Any, JsonValue])
			}
		}
		it ("Appends a 101th value") {
			assertResult(Success(hexSeq"""9903e9 B7A8B7A8B7A8 60""")){
				new CborArrayBuilder().apply("", hexSeq"""9903e8 B7A8B7A8B7A8 """, JsonValue(""), new IdentityParser[Any, JsonValue])
			}
		}
		it ("Appends a 256th value") {
			assertResult(Success(hexSeq"""990100 B7A8B7A8B7A8 60""")){
				new CborArrayBuilder().apply("", hexSeq"""98FF B7A8B7A8B7A8 """, JsonValue(""), new IdentityParser[Any, JsonValue])
			}
		}
		it ("Appends a 65536th value") {
			assertResult(Success(hexSeq"""9A00010000 B7A8B7A8B7A8 60""")){
				new CborArrayBuilder().apply("", hexSeq"""99FFFF B7A8B7A8B7A8 """, JsonValue(""), new IdentityParser[Any, JsonValue])
			}
		}
		it ("Appends a 1000001th value") {
			assertResult(Success(hexSeq"""9a000f4241 B7A8B7A8B7A8 60""")){
				new CborArrayBuilder().apply("", hexSeq"""9a000f4240 B7A8B7A8B7A8 """, JsonValue(""), new IdentityParser[Any, JsonValue])
			}
		}
	}
	
	describe("CborArrayBuilder integration") {
		import com.rayrobdod.json.parser.{JsonParser, SeqParser, CborParser, CaseClassParser, byteArray2DataInput}
		
		it ("CborArrayBuilder + JsonParser + primitive") {
			assertResult(hexSeq"83 183D 183E 183F"){
				new JsonParser().parse(
					new CborArrayBuilder().mapKey[StringOrInt].mapValue[JsonValue],
					"""[61, 62, 63]"""
				).get.left.get
			}
		}
		ignore ("CborArrayBuilder + JsonParser + nested objects") {
			assertResult(hexSeq"81 A2 616100 616201"){
				new JsonParser().parse(
					new CborArrayBuilder().mapKey[StringOrInt].mapValue[JsonValue],
					"""[{"a":0,"b":1}]"""
				).get.left.get
			}
		}
		it ("CborArrayBuilder + JsonParser + nested arrays") {
			assertResult(hexSeq"81 82 00 01"){
				new JsonParser().parse(
					new CborArrayBuilder().mapKey[StringOrInt].mapValue[JsonValue],
					"""[[0,1]]"""
				).get.left.get
			}
		}
		it ("CborArrayBuilder + CborParser + primitives") {
			assertResult(hexSeq"8262202005"){
				new CborParser().parse(
					new CborArrayBuilder().mapKey[JsonValue].mapValue[JsonValue],
					byteArray2DataInput(
							hexArray"8262202005"
					)
				).get.left.get
			}
		}
	}
}
