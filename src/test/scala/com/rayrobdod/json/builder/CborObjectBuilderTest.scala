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
import org.scalatest.FunSpec;
import java.nio.charset.StandardCharsets.US_ASCII;
import com.rayrobdod.json.union.JsonValue
import com.rayrobdod.json.union.CborValue
import com.rayrobdod.json.union.StringOrInt
import com.rayrobdod.json.union.CborValue._
import com.rayrobdod.json.parser.IdentityParser
import com.rayrobdod.json.parser.FailureParser
import com.rayrobdod.json.parser.{byteArray2DataInput, HexArrayStringConverter}

@deprecated("CborObjectBuilder is deprecated; using to suppress warnings tests related to that class", "3.0")
class CborObjectBuilderTest extends FunSpec {
	private case class Abc(a:Int, b:Boolean, c:String)
	private implicit def classAbc = classOf[Abc]
	
	describe("CborObjectBuilder") {
		it ("inits correctly") {
			assertResult(Seq(0xA0.byteValue)){new CborObjectBuilder().init}
		}
		it ("Appends null") {
			assertResult(Right(hexSeq"A160f6")){
				new CborObjectBuilder().apply(hexSeq"A0", CborValue(""), CborValueNull, new IdentityParser[CborValue])
			}
		}
		it ("Appends true") {
			assertResult(Right(hexSeq"A160f5")){
				new CborObjectBuilder().apply(hexSeq"A0", CborValue(""), CborValue(true), new IdentityParser[CborValue])
			}
		}
		it ("Appends false") {
			assertResult(Right(hexSeq"A160f4")){
				new CborObjectBuilder().apply(hexSeq"A0", CborValue(""), CborValue(false), new IdentityParser[CborValue])
			}
		}
		it ("Appends 0 as one byte") {
			assertResult(Right(hexSeq"A16000")){
				new CborObjectBuilder().apply(hexSeq"A0", CborValue(""), CborValue(0), new IdentityParser[CborValue])
			}
		}
		it ("Appends small positive integer") {
			assertResult(Right(hexSeq"A16006")){
				new CborObjectBuilder().apply(hexSeq"A0", CborValue(""), CborValue(6), new IdentityParser[CborValue])
			}
		}
		it ("Appends 23 as one byte") {
			assertResult(Right(hexSeq"A16017")){
				new CborObjectBuilder().apply(hexSeq"A0", CborValue(""), CborValue(23), new IdentityParser[CborValue])
			}
		}
		it ("Appends 24 as two bytes") {
			assertResult(Right(hexSeq"A1601818")){
				new CborObjectBuilder().apply(hexSeq"A0", CborValue(""), CborValue(24), new IdentityParser[CborValue])
			}
		}
		it ("Appends 255 as two bytes") {
			assertResult(Right(hexSeq"A16018FF")){
				new CborObjectBuilder().apply(hexSeq"A0", CborValue(""), CborValue(255), new IdentityParser[CborValue])
			}
		}
		it ("Appends 256 as three bytes") {
			assertResult(Right(hexSeq"A160190100")){
				new CborObjectBuilder().apply(hexSeq"A0", CborValue(""), CborValue(256), new IdentityParser[CborValue])
			}
		}
		it ("Appends -1 as one byte") {
			assertResult(Right(hexSeq"A16020")){
				new CborObjectBuilder().apply(hexSeq"A0", CborValue(""), CborValue(-1), new IdentityParser[CborValue])
			}
		}
		it ("Appends -100 as two bytes") {
			assertResult(Right(hexSeq"A1603863")){
				new CborObjectBuilder().apply(hexSeq"A0", CborValue(""), CborValue(-100), new IdentityParser[CborValue])
			}
		}
		it ("Appends Float") {
			assertResult(Right(hexSeq"A160 FA3FC00000")){
				new CborObjectBuilder().apply(hexSeq"A0", CborValue(""), CborValue(1.5f), new IdentityParser[CborValue])
			}
		}
		it ("Appends Double") {
			assertResult(Right(hexSeq"A160 fbc010666666666666")){
				new CborObjectBuilder().apply(hexSeq"A0", CborValue(""), CborValue(-4.1D), new IdentityParser[CborValue])
			}
		}
		it ("Appends empty string") {
			assertResult(Right(hexSeq"A16060")){
				new CborObjectBuilder().apply(hexSeq"A0", CborValue(""), CborValue(""), new IdentityParser[CborValue])
			}
		}
		it ("Appends \"abc\"") {
			assertResult(Right(hexSeq"A16063616263")){
				new CborObjectBuilder().apply(hexSeq"A0", CborValue(""), CborValue("abc"), new IdentityParser[CborValue])
			}
		}
		it ("Appends string containing multibyte char") {
			assertResult(Right(hexSeq"A16063e6b0b4")){
				new CborObjectBuilder().apply(hexSeq"A0", CborValue(""), CborValue("\u6c34"), new IdentityParser[CborValue])
			}
		}
		it ("Appends an empty byte array") {
			assertResult(Right(hexSeq"A16040")){
				new CborObjectBuilder().apply(hexSeq"A0", CborValue(""), CborValue(new Array[Byte](0)), new IdentityParser[CborValue])
			}
		}
		it ("Appends a non-empty byte array") {
			assertResult(Right(hexSeq"A16043010203")){
				new CborObjectBuilder().apply(hexSeq"A0", CborValue(""), CborValue(hexArray"010203"), new IdentityParser[CborValue])
			}
		}
		it ("Appends a second value") {
			assertResult(Right(hexSeq"""A26161616261636164""")){
				new CborObjectBuilder().apply(hexSeq"A161616162", CborValue("c"), CborValue("d"), new IdentityParser[CborValue])
			}
		}
		it ("Appends a 24th value") {
			assertResult(Right(hexSeq"""B818 B7A8B7A8B7A8 6060""")){
				new CborObjectBuilder().apply(hexSeq"""B7 B7A8B7A8B7A8 """, CborValue(""), CborValue(""), new IdentityParser[CborValue])
			}
		}
		it ("Appends a 101th value") {
			assertResult(Right(hexSeq"""B903e9 B7A8B7A8B7A8 6060""")){
				new CborObjectBuilder().apply(hexSeq"""B903e8 B7A8B7A8B7A8""", CborValue(""), CborValue(""), new IdentityParser[CborValue])
			}
		}
		it ("Appends a 256th value") {
			assertResult(Right(hexSeq"""B90100 B7A8B7A8B7A8 6060""")){
				new CborObjectBuilder().apply(hexSeq"""B8FF B7A8B7A8B7A8""", CborValue(""), CborValue(""), new IdentityParser[CborValue])
			}
		}
		it ("Appends a 65536th value") {
			assertResult(Right(hexSeq"""BA00010000 B7A8B7A8B7A8 6060""")){
				new CborObjectBuilder().apply(hexSeq"""B9FFFF B7A8B7A8B7A8""", CborValue(""), CborValue(""), new IdentityParser[CborValue])
			}
		}
		it ("Appends a 1000001th value") {
			assertResult(Right(hexSeq"""Ba000f4241 B7A8B7A8B7A8 6060""")){
				new CborObjectBuilder().apply(hexSeq"""Ba000f4240 B7A8B7A8B7A8""", CborValue(""), CborValue(""), new IdentityParser[CborValue])
			}
		}
		
		it ("When parser reports a failure, the failure is forwarded") {
			assertResult( Left("FailureParser", 0) ){
				new CborObjectBuilder().apply(hexSeq"A0", "", "", new FailureParser)
			}
		}
	}
	
	describe("CborObjectBuilder integration") {
		import com.rayrobdod.json.parser.{JsonParser, CborParser, CaseClassParser}
		
		it ("CborObjectBuilder + JsonParser + primitive") {
			assertResult(hexSeq"A3 6161 183D 6162 183E 6163 183F"){
				new JsonParser().parse(
					new CborObjectBuilder().mapKey[StringOrInt].mapValue[JsonValue],
					"""{"a":61,"b":62,"c":63}"""
				).fold({x => x}, {x => x}, {(s,i) => ((s,i))})
			}
		}
		it ("CborObjectBuilder + JsonParser + nested objects") {
			assertResult(hexSeq"A1 60 A2 616100 616201"){
				new JsonParser().parse(
					new CborObjectBuilder().mapKey[StringOrInt].mapValue[JsonValue],
					"""{"":{"a":0,"b":1}}"""
				).fold({x => x}, {x => x}, {(s,i) => ((s,i))})
			}
		}
		it ("CborObjectBuilder + CborParser + primitive key") {
			assertResult(hexSeq"A10405"){
				new CborParser().parse(
					new CborObjectBuilder().mapValue[CborValue],
					byteArray2DataInput(
							hexArray"A10405"
					)
				).fold({x => x}, {x => x}, {(s,i) => ((s,i))})
			}
		}
		it ("CborObjectBuilder + CborParser + primitives") {
			assertResult(hexSeq"A162202005"){
				new CborParser().parse(
					new CborObjectBuilder().mapValue[CborValue],
					byteArray2DataInput(
							hexArray"A162202005"
					)
				).fold({x => x}, {x => x}, {(s,i) => ((s,i))})
			}
		}
		it ("CborObjectBuilder + CaseClassParser") {
			assertResult(hexSeq"A3 6161 05 6162 f4 6163 63737472"){
				new CaseClassParser[Abc]().parse(
					new CborObjectBuilder().mapKey[String].mapValue[Any]{CborValue.unsafeWrap _},
					Abc(5,false,"str")
				).fold({x => x}, {x => x}, {(s,i) => ((s,i))})
			}
		}
	}
}
