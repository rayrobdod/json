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

import scala.collection.immutable.Seq
import org.scalatest.FunSpec;
import com.rayrobdod.json.union.StringOrInt
import com.rayrobdod.json.union.JsonValue
import com.rayrobdod.json.union.CborValue
import com.rayrobdod.json.union.CborValue._
import com.rayrobdod.json.parser.IdentityParser
import com.rayrobdod.json.parser.FailureParser
import com.rayrobdod.json.testing.HexArrayStringConverter
import scala.language.implicitConversions

class CborBuilderTest extends FunSpec {
	private implicit def integer2CborValue(x:Int):CborValue = CborValue(x)
	
	
	describe("CborBuilder") {
		it ("inits correctly") {
			assertResult(Seq(0x80.byteValue)){new CborBuilder().init}
		}
		it ("inits correctly (object)") {
			assertResult(Seq(0xA0.byteValue)){new CborBuilder(true).init}
		}
		it ("Appends null to empty array") {
			assertResult(Right(hexSeq"81f6")){
				new CborBuilder().apply(hexSeq"80", 0, CborValueNull, new IdentityParser[CborValue])
			}
		}
		it ("Appends true") {
			assertResult(Right(hexSeq"81f5")){
				new CborBuilder().apply(hexSeq"80", 0, CborValue(true), new IdentityParser[CborValue])
			}
		}
		it ("Appends false") {
			assertResult(Right(hexSeq"81f4")){
				new CborBuilder().apply(hexSeq"80", 0, CborValue(false), new IdentityParser[CborValue])
			}
		}
		it ("Appends 0 as one byte") {
			assertResult(Right(hexSeq"8100")){
				new CborBuilder().apply(hexSeq"80", 0, CborValue(0), new IdentityParser[CborValue])
			}
		}
		it ("Appends small positive integer") {
			assertResult(Right(hexSeq"8106")){
				new CborBuilder().apply(hexSeq"80", 0, CborValue(6), new IdentityParser[CborValue])
			}
		}
		it ("Appends 23 as one byte") {
			assertResult(Right(hexSeq"8117")){
				new CborBuilder().apply(hexSeq"80", 0, CborValue(23), new IdentityParser[CborValue])
			}
		}
		it ("Appends 24 as two bytes") {
			assertResult(Right(hexSeq"811818")){
				new CborBuilder().apply(hexSeq"80", 0, CborValue(24), new IdentityParser[CborValue])
			}
		}
		it ("Appends 255 as two bytes") {
			assertResult(Right(hexSeq"8118FF")){
				new CborBuilder().apply(hexSeq"80", 0, CborValue(255), new IdentityParser[CborValue])
			}
		}
		it ("Appends 256 as three bytes") {
			assertResult(Right(hexSeq"81190100")){
				new CborBuilder().apply(hexSeq"80", 0, CborValue(256), new IdentityParser[CborValue])
			}
		}
		it ("Appends -1 as one byte") {
			assertResult(Right(hexSeq"8120")){
				new CborBuilder().apply(hexSeq"80", 0, CborValue(-1), new IdentityParser[CborValue])
			}
		}
		it ("Appends -100 as two bytes") {
			assertResult(Right(hexSeq"813863")){
				new CborBuilder().apply(hexSeq"80", 0, CborValue(-100), new IdentityParser[CborValue])
			}
		}
		it ("Appends Float") {
			assertResult(Right(hexSeq"81 FA3FC00000")){
				new CborBuilder().apply(hexSeq"80", 0, CborValue(1.5f), new IdentityParser[CborValue])
			}
		}
		it ("Appends Double") {
			assertResult(Right(hexSeq"81 fbc010666666666666")){
				new CborBuilder().apply(hexSeq"80", 0, CborValue(-4.1D), new IdentityParser[CborValue])
			}
		}
		it ("Appends empty string") {
			assertResult(Right(hexSeq"8160")){
				new CborBuilder().apply(hexSeq"80", 0, CborValue(""), new IdentityParser[CborValue])
			}
		}
		it ("Appends \"abc\"") {
			assertResult(Right(hexSeq"8163616263")){
				new CborBuilder().apply(hexSeq"80", 0, CborValue("abc"), new IdentityParser[CborValue])
			}
		}
		it ("Appends string containing multibyte char") {
			assertResult(Right(hexSeq"8163e6b0b4")){
				new CborBuilder().apply(hexSeq"80", 0, CborValue("\u6c34"), new IdentityParser[CborValue])
			}
		}
		it ("Appends an empty byte array") {
			assertResult(Right(hexSeq"8140")){
				new CborBuilder().apply(hexSeq"80", 0, CborValue(new Array[Byte](0)), new IdentityParser[CborValue])
			}
		}
		it ("Appends a non-empty byte array") {
			assertResult(Right(hexSeq"8143010203")){
				new CborBuilder().apply(hexSeq"80", 0, CborValue(hexArray"010203"), new IdentityParser[CborValue])
			}
		}
		it ("Appends a second value") {
			assertResult(Right(hexSeq"""83616161626164""")){
				new CborBuilder().apply(hexSeq"""8261616162""", 2, CborValue("d"), new IdentityParser[CborValue])
			}
		}
		it ("Appends a 24th value") {
			assertResult(Right(hexSeq"""9818 B7A8B7A8B7A8 60""")){
				new CborBuilder().apply(hexSeq"""97 B7A8B7A8B7A8 """, 23, CborValue(""), new IdentityParser[CborValue])
			}
		}
		it ("Appends a 256th value") {
			assertResult(Right(hexSeq"""990100 B7A8B7A8B7A8 60""")){
				new CborBuilder().apply(hexSeq"""98FF B7A8B7A8B7A8 """, 255, CborValue(""), new IdentityParser[CborValue])
			}
		}
		it ("Appends a 1001th value") {
			assertResult(Right(hexSeq"""9903e9 B7A8B7A8B7A8 60""")){
				new CborBuilder().apply(hexSeq"""9903e8 B7A8B7A8B7A8 """, 1000, CborValue(""), new IdentityParser[CborValue])
			}
		}
		it ("Appends a 65536th value") {
			assertResult(Right(hexSeq"""9A00010000 B7A8B7A8B7A8 60""")){
				new CborBuilder().apply(hexSeq"""99FFFF B7A8B7A8B7A8 """, 65535, CborValue(""), new IdentityParser[CborValue])
			}
		}
		it ("Appends a 1000001th value") {
			assertResult(Right(hexSeq"""9a000f4241 B7A8B7A8B7A8 60""")){
				new CborBuilder().apply(hexSeq"""9a000f4240 B7A8B7A8B7A8 """, 1000000, CborValue(""), new IdentityParser[CborValue])
			}
		}
		
		
		it ("Appends null to empty object") {
			assertResult(Right(hexSeq"A100f6")){
				new CborBuilder().apply(hexSeq"A0", 0, CborValueNull, new IdentityParser[CborValue])
			}
		}
		it ("Converts an empty array into an object when given a non-zero key") {
			assertResult(Right(hexSeq"A160f6")){
				new CborBuilder().apply(hexSeq"80", "", CborValueNull, new IdentityParser[CborValue])
			}
		}
		it ("Converts an empty array into an object when given a non-zero key (2)") {
			assertResult(Right(hexSeq"A106f6")){
				new CborBuilder().apply(hexSeq"80", 6, CborValueNull, new IdentityParser[CborValue])
			}
		}
		it ("Converts a two-element array into an object when given a non-2 key") {
			assertResult(Right(hexSeq"""A3 006161 016162 056164""")){
				new CborBuilder().apply(hexSeq"""8261616162""", 5, CborValue("d"), new IdentityParser[CborValue])
			}
		}
		it ("Appends a 1001th value to an object") {
			assertResult(Right(hexSeq"""B903e9 B7A8B7A8B7A8 6060""")){
				new CborBuilder().apply(hexSeq"""B903e8 B7A8B7A8B7A8 """, "", CborValue(""), new IdentityParser[CborValue])
			}
		}
		it ("Fails to convert an array to an object when unexpected key and illegal folding object") {
			assertResult(Left("Not a public value", 0)){
				new CborBuilder().apply(hexSeq"81FF", 6, CborValueNull, new IdentityParser[CborValue])
			}
		}
		
		
		it ("Refuses to fold a non-array or non-object") {
			assertResult(Left("Invalid folding parameter", 0)){
				new CborBuilder().apply(hexSeq"""00""", "", CborValue(""), new IdentityParser[CborValue])
			}
		}
		it ("When parser reports a failure, the failure is forwarded") {
			assertResult( ("FailureParser", 0) ){
				new CborBuilder().apply(hexSeq"80 THEREST", "", "", new FailureParser).left.get
			}
		}
	}
	
	describe("CborBuilder integration") {
		import com.rayrobdod.json.parser.{JsonParser, CborParser, byteArray2DataInput}
		
		it ("CborBuilder + JsonParser + primitive (array)") {
			assertResult(hexSeq"83 183D 183E 183F"){
				new JsonParser().parse(
					new CborBuilder().mapKey[StringOrInt].mapValue[JsonValue],
					"""[61, 62, 63]"""
				).fold({x => x}, {x => x}, {(s,i) => ((s,i))})
			}
		}
		it ("CborBuilder + JsonParser + nested objects (array)") {
			assertResult(hexSeq"81 A2 616100 616201"){
				new JsonParser().parse(
					new CborBuilder().mapKey[StringOrInt].mapValue[JsonValue],
					"""[{"a":0,"b":1}]"""
				).fold({x => x}, {x => x}, {(s,i) => ((s,i))})
			}
		}
		it ("CborBuilder + JsonParser + nested arrays (array)") {
			assertResult(hexSeq"81 82 00 01"){
				new JsonParser().parse(
					new CborBuilder().mapKey[StringOrInt].mapValue[JsonValue],
					"""[[0,1]]"""
				).fold({x => x}, {x => x}, {(s,i) => ((s,i))})
			}
		}
		it ("CborBuilder + CborParser + primitives (array)") {
			assertResult(hexSeq"8262202005"){
				new CborParser().parse(
					new CborBuilder(),
					byteArray2DataInput(
							hexArray"8262202005"
					)
				).fold({x => x}, {x => x}, {(s,i) => ((s,i))})
			}
		}
		
		
		it ("CborBuilder + JsonParser + primitive (object)") {
			assertResult(hexSeq"A3 6161 183D 6162 183E 6163 183F"){
				new JsonParser().parse(
					new CborBuilder().mapKey[StringOrInt].mapValue[JsonValue],
					"""{"a":61,"b":62,"c":63}"""
				).fold({x => x}, {x => x}, {(s,i) => ((s,i))})
			}
		}
		it ("CborBuilder + JsonParser + nested objects (object)") {
			assertResult(hexSeq"A1 60 A2 616100 616201"){
				new JsonParser().parse(
					new CborBuilder().mapKey[StringOrInt].mapValue[JsonValue],
					"""{"":{"a":0,"b":1}}"""
				).fold({x => x}, {x => x}, {(s,i) => ((s,i))})
			}
		}
		it ("CborBuilder + CborParser + primitive key (object)") {
			assertResult(hexSeq"A10405"){
				new CborParser().parse(
					new CborBuilder().mapValue[CborValue],
					byteArray2DataInput(
							hexArray"A10405"
					)
				).fold({x => x}, {x => x}, {(s,i) => ((s,i))})
			}
		}
		it ("CborBuilder + CborParser + primitives (object)") {
			assertResult(hexSeq"A162202005"){
				new CborParser().parse(
					new CborBuilder().mapValue[CborValue],
					byteArray2DataInput(
							hexArray"A162202005"
					)
				).fold({x => x}, {x => x}, {(s,i) => ((s,i))})
			}
		}
		it ("CborBuilder + CaseClassParser (object)") {
			assertResult(hexSeq"A3 6161 05 6162 f4 6163 63737472"){
				val builder = new CborBuilder().mapKey[String]
				builder.apply(builder.init, "a", CborValue(5), new IdentityParser[CborValue]).right.flatMap{a =>
					builder.apply(a, "b", CborValue(false), new IdentityParser[CborValue]).right.flatMap{b =>
						builder.apply(b, "c", CborValue("str"), new IdentityParser[CborValue])
					}
				}.fold({si => si}, {x => x})
			}
		}
	}
}
