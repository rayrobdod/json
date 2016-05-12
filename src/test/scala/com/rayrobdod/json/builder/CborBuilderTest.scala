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

import java.text.ParseException;
import scala.collection.immutable.{Seq, Map};
import scala.util.{Try, Success, Failure}
import org.scalatest.FunSpec;
import com.rayrobdod.json.union.StringOrInt
import com.rayrobdod.json.union.JsonValue
import com.rayrobdod.json.union.JsonValue._
import com.rayrobdod.json.parser.IdentityParser
import com.rayrobdod.json.parser.HexArrayStringConverter;
import scala.language.implicitConversions

class CborBuilderTest extends FunSpec {
	private case class Abc(a:Int, b:Boolean, c:String)
	private implicit def classAbc = classOf[Abc]
	private implicit def integer2JsonValue(x:Int):JsonValue = JsonValue(x)
	
	
	describe("CborBuilder") {
		it ("inits correctly") {
			assertResult(Seq(0x80.byteValue)){new CborBuilder().init}
		}
		it ("inits correctly (object)") {
			assertResult(Seq(0xA0.byteValue)){new CborBuilder(true).init}
		}
		it ("Appends null to empty array") {
			assertResult(Success(hexSeq"81f6")){
				new CborBuilder().apply(hexSeq"80", 0, JsonValueNull, new IdentityParser[JsonValue, JsonValue])
			}
		}
		it ("Appends true") {
			assertResult(Success(hexSeq"81f5")){
				new CborBuilder().apply(hexSeq"80", 0, JsonValue(true), new IdentityParser[JsonValue, JsonValue])
			}
		}
		it ("Appends false") {
			assertResult(Success(hexSeq"81f4")){
				new CborBuilder().apply(hexSeq"80", 0, JsonValue(false), new IdentityParser[JsonValue, JsonValue])
			}
		}
		it ("Appends 0 as one byte") {
			assertResult(Success(hexSeq"8100")){
				new CborBuilder().apply(hexSeq"80", 0, JsonValue(0), new IdentityParser[JsonValue, JsonValue])
			}
		}
		it ("Appends small positive integer") {
			assertResult(Success(hexSeq"8106")){
				new CborBuilder().apply(hexSeq"80", 0, JsonValue(6), new IdentityParser[JsonValue, JsonValue])
			}
		}
		it ("Appends 23 as one byte") {
			assertResult(Success(hexSeq"8117")){
				new CborBuilder().apply(hexSeq"80", 0, JsonValue(23), new IdentityParser[JsonValue, JsonValue])
			}
		}
		it ("Appends 24 as two bytes") {
			assertResult(Success(hexSeq"811818")){
				new CborBuilder().apply(hexSeq"80", 0, JsonValue(24), new IdentityParser[JsonValue, JsonValue])
			}
		}
		it ("Appends 255 as two bytes") {
			assertResult(Success(hexSeq"8118FF")){
				new CborBuilder().apply(hexSeq"80", 0, JsonValue(255), new IdentityParser[JsonValue, JsonValue])
			}
		}
		it ("Appends 256 as three bytes") {
			assertResult(Success(hexSeq"81190100")){
				new CborBuilder().apply(hexSeq"80", 0, JsonValue(256), new IdentityParser[JsonValue, JsonValue])
			}
		}
		it ("Appends -1 as one byte") {
			assertResult(Success(hexSeq"8120")){
				new CborBuilder().apply(hexSeq"80", 0, JsonValue(-1), new IdentityParser[JsonValue, JsonValue])
			}
		}
		it ("Appends -100 as two bytes") {
			assertResult(Success(hexSeq"813863")){
				new CborBuilder().apply(hexSeq"80", 0, JsonValue(-100), new IdentityParser[JsonValue, JsonValue])
			}
		}
		it ("Appends Float") {
			assertResult(Success(hexSeq"81 FA3FC00000")){
				new CborBuilder().apply(hexSeq"80", 0, JsonValue(1.5f), new IdentityParser[JsonValue, JsonValue])
			}
		}
		it ("Appends Double") {
			assertResult(Success(hexSeq"81 fbc010666666666666")){
				new CborBuilder().apply(hexSeq"80", 0, JsonValue(-4.1D), new IdentityParser[JsonValue, JsonValue])
			}
		}
		it ("Appends empty string") {
			assertResult(Success(hexSeq"8160")){
				new CborBuilder().apply(hexSeq"80", 0, JsonValue(""), new IdentityParser[JsonValue, JsonValue])
			}
		}
		it ("Appends \"abc\"") {
			assertResult(Success(hexSeq"8163616263")){
				new CborBuilder().apply(hexSeq"80", 0, JsonValue("abc"), new IdentityParser[JsonValue, JsonValue])
			}
		}
		it ("Appends string containing multibyte char") {
			assertResult(Success(hexSeq"8163e6b0b4")){
				new CborBuilder().apply(hexSeq"80", 0, JsonValue("\u6c34"), new IdentityParser[JsonValue, JsonValue])
			}
		}
		it ("Appends an empty byte array") {
			assertResult(Success(hexSeq"8140")){
				new CborBuilder().apply(hexSeq"80", 0, JsonValue(new Array[Byte](0)), new IdentityParser[JsonValue, JsonValue])
			}
		}
		it ("Appends a non-empty byte array") {
			assertResult(Success(hexSeq"8143010203")){
				new CborBuilder().apply(hexSeq"80", 0, JsonValue(hexArray"010203"), new IdentityParser[JsonValue, JsonValue])
			}
		}
		it ("Appends a second value") {
			assertResult(Success(hexSeq"""83616161626164""")){
				new CborBuilder().apply(hexSeq"""8261616162""", 2, JsonValue("d"), new IdentityParser[JsonValue, JsonValue])
			}
		}
		it ("Appends a 24th value") {
			assertResult(Success(hexSeq"""9818 B7A8B7A8B7A8 60""")){
				new CborBuilder().apply(hexSeq"""97 B7A8B7A8B7A8 """, 23, JsonValue(""), new IdentityParser[JsonValue, JsonValue])
			}
		}
		it ("Appends a 256th value") {
			assertResult(Success(hexSeq"""990100 B7A8B7A8B7A8 60""")){
				new CborBuilder().apply(hexSeq"""98FF B7A8B7A8B7A8 """, 255, JsonValue(""), new IdentityParser[JsonValue, JsonValue])
			}
		}
		it ("Appends a 1001th value") {
			assertResult(Success(hexSeq"""9903e9 B7A8B7A8B7A8 60""")){
				new CborBuilder().apply(hexSeq"""9903e8 B7A8B7A8B7A8 """, 1000, JsonValue(""), new IdentityParser[JsonValue, JsonValue])
			}
		}
		it ("Appends a 65536th value") {
			assertResult(Success(hexSeq"""9A00010000 B7A8B7A8B7A8 60""")){
				new CborBuilder().apply(hexSeq"""99FFFF B7A8B7A8B7A8 """, 65535, JsonValue(""), new IdentityParser[JsonValue, JsonValue])
			}
		}
		it ("Appends a 1000001th value") {
			assertResult(Success(hexSeq"""9a000f4241 B7A8B7A8B7A8 60""")){
				new CborBuilder().apply(hexSeq"""9a000f4240 B7A8B7A8B7A8 """, 1000000, JsonValue(""), new IdentityParser[JsonValue, JsonValue])
			}
		}
		
		
		it ("Appends null to empty object") {
			assertResult(Success(hexSeq"A100f6")){
				new CborBuilder().apply(hexSeq"A0", 0, JsonValueNull, new IdentityParser[JsonValue, JsonValue])
			}
		}
		it ("Converts an empty array into an object when given a non-zero key") {
			assertResult(Success(hexSeq"A160f6")){
				new CborBuilder().apply(hexSeq"80", "", JsonValueNull, new IdentityParser[JsonValue, JsonValue])
			}
		}
		it ("Converts an empty array into an object when given a non-zero key (2)") {
			assertResult(Success(hexSeq"A106f6")){
				new CborBuilder().apply(hexSeq"80", 6, JsonValueNull, new IdentityParser[JsonValue, JsonValue])
			}
		}
		it ("Converts a two-element array into an object when given a non-2 key") {
			assertResult(Success(hexSeq"""A3 006161 016162 056164""")){
				new CborBuilder().apply(hexSeq"""8261616162""", 5, JsonValue("d"), new IdentityParser[JsonValue, JsonValue])
			}
		}
		it ("Appends a 1001th value to an object") {
			assertResult(Success(hexSeq"""B903e9 B7A8B7A8B7A8 6060""")){
				new CborBuilder().apply(hexSeq"""B903e8 B7A8B7A8B7A8 """, "", JsonValue(""), new IdentityParser[JsonValue, JsonValue])
			}
		}
		
		
		it ("Refuses to fold a non-array or non-object") {
			assertFailureParse("Invalid folding parameter", 0){
				new CborBuilder().apply(hexSeq"""00""", "", JsonValue(""), new IdentityParser[JsonValue, JsonValue])
			}
		}
	}
	
	describe("CborBuilder integration") {
		import com.rayrobdod.json.parser.{JsonParser, SeqParser, CborParser, CaseClassParser, byteArray2DataInput}
		
		it ("CborBuilder + JsonParser + primitive (array)") {
			assertResult(hexSeq"83 183D 183E 183F"){
				new JsonParser().parse(
					new CborBuilder().mapKey[StringOrInt].mapValue[JsonValue],
					"""[61, 62, 63]"""
				).get.left.get
			}
		}
		it ("CborBuilder + JsonParser + nested objects (array)") {
			assertResult(hexSeq"81 A2 616100 616201"){
				new JsonParser().parse(
					new CborBuilder().mapKey[StringOrInt].mapValue[JsonValue],
					"""[{"a":0,"b":1}]"""
				).get.left.get
			}
		}
		it ("CborBuilder + JsonParser + nested arrays (array)") {
			assertResult(hexSeq"81 82 00 01"){
				new JsonParser().parse(
					new CborBuilder().mapKey[StringOrInt].mapValue[JsonValue],
					"""[[0,1]]"""
				).get.left.get
			}
		}
		it ("CborBuilder + CborParser + primitives (array)") {
			assertResult(hexSeq"8262202005"){
				new CborParser().parse(
					new CborBuilder(),
					byteArray2DataInput(
							hexArray"8262202005"
					)
				).get.left.get
			}
		}
		
		
		it ("CborBuilder + JsonParser + primitive (object)") {
			assertResult(hexSeq"A3 6161 183D 6162 183E 6163 183F"){
				new JsonParser().parse(
					new CborBuilder().mapKey[StringOrInt].mapValue[JsonValue],
					"""{"a":61,"b":62,"c":63}"""
				).get.left.get
			}
		}
		it ("CborBuilder + JsonParser + nested objects (object)") {
			assertResult(hexSeq"A1 60 A2 616100 616201"){
				new JsonParser().parse(
					new CborBuilder().mapKey[StringOrInt].mapValue[JsonValue],
					"""{"":{"a":0,"b":1}}"""
				).get.left.get
			}
		}
		it ("CborBuilder + CborParser + primitive key (object)") {
			assertResult(hexSeq"A10405"){
				new CborParser().parse(
					new CborBuilder().mapValue[JsonValue],
					byteArray2DataInput(
							hexArray"A10405"
					)
				).get.left.get
			}
		}
		it ("CborBuilder + CborParser + primitives (object)") {
			assertResult(hexSeq"A162202005"){
				new CborParser().parse(
					new CborBuilder().mapValue[JsonValue],
					byteArray2DataInput(
							hexArray"A162202005"
					)
				).get.left.get
			}
		}
		it ("CborBuilder + CaseClassParser (object)") {
			assertResult(hexSeq"A3 6161 05 6162 f4 6163 63737472"){
				new CaseClassParser[Abc]().parse(
					new CborBuilder().mapKey[String].mapValue[Any]{JsonValue.unsafeWrap _},
					Abc(5,false,"str")
				).get.left.get
			}
		}
	}
	
	
	def assertFailureParse(msg:String, idx:Int)(result:Try[_]):Unit = result match {
		case Failure(x:ParseException) => {
			assertResult(msg){x.getMessage}
			assertResult(idx){x.getErrorOffset}
		}
		case Failure(x) => fail("Not a ParseException: " + x)
		case x => fail("Not a Failure: " + x)
	}
}
