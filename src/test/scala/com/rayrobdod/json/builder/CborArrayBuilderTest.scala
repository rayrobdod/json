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
import scala.collection.immutable.{Seq, Map};
import org.scalatest.FunSpec;
import java.nio.charset.StandardCharsets.US_ASCII;
import com.rayrobdod.json.union.StringOrInt
import com.rayrobdod.json.union.CborValue._
import com.rayrobdod.json.union.CborValue
import com.rayrobdod.json.union.JsonValue
import com.rayrobdod.json.parser.IdentityParser
import com.rayrobdod.json.parser.FailureParser
import com.rayrobdod.json.parser.HexArrayStringConverter;

class CborArrayBuilderTest extends FunSpec {
	private case class Abc(a:Int, b:Boolean, c:String)
	private implicit def classAbc = classOf[Abc]
	
	
	describe("CborArrayBuilder") {
		it ("inits correctly") {
			assertResult(Seq(0x80.byteValue)){new CborArrayBuilder().init}
		}
		it ("Appends null") {
			assertResult(Right(hexSeq"81f6")){
				new CborArrayBuilder().apply(hexSeq"80", "", CborValueNull, new IdentityParser[Any, CborValue])
			}
		}
		it ("Appends true") {
			assertResult(Right(hexSeq"81f5")){
				new CborArrayBuilder().apply(hexSeq"80", "", CborValue(true), new IdentityParser[Any, CborValue])
			}
		}
		it ("Appends false") {
			assertResult(Right(hexSeq"81f4")){
				new CborArrayBuilder().apply(hexSeq"80", "", CborValue(false), new IdentityParser[Any, CborValue])
			}
		}
		it ("Appends 0 as one byte") {
			assertResult(Right(hexSeq"8100")){
				new CborArrayBuilder().apply(hexSeq"80", "", CborValue(0), new IdentityParser[Any, CborValue])
			}
		}
		it ("Appends small positive integer") {
			assertResult(Right(hexSeq"8106")){
				new CborArrayBuilder().apply(hexSeq"80", "", CborValue(6), new IdentityParser[Any, CborValue])
			}
		}
		it ("Appends 23 as one byte") {
			assertResult(Right(hexSeq"8117")){
				new CborArrayBuilder().apply(hexSeq"80", "", CborValue(23), new IdentityParser[Any, CborValue])
			}
		}
		it ("Appends 24 as two bytes") {
			assertResult(Right(hexSeq"811818")){
				new CborArrayBuilder().apply(hexSeq"80", "", CborValue(24), new IdentityParser[Any, CborValue])
			}
		}
		it ("Appends 255 as two bytes") {
			assertResult(Right(hexSeq"8118FF")){
				new CborArrayBuilder().apply(hexSeq"80", "", CborValue(255), new IdentityParser[Any, CborValue])
			}
		}
		it ("Appends 256 as three bytes") {
			assertResult(Right(hexSeq"81190100")){
				new CborArrayBuilder().apply(hexSeq"80", "", CborValue(256), new IdentityParser[Any, CborValue])
			}
		}
		it ("Appends -1 as one byte") {
			assertResult(Right(hexSeq"8120")){
				new CborArrayBuilder().apply(hexSeq"80", "", CborValue(-1), new IdentityParser[Any, CborValue])
			}
		}
		it ("Appends -100 as two bytes") {
			assertResult(Right(hexSeq"813863")){
				new CborArrayBuilder().apply(hexSeq"80", "", CborValue(-100), new IdentityParser[Any, CborValue])
			}
		}
		it ("Appends Float") {
			assertResult(Right(hexSeq"81 FA3FC00000")){
				new CborArrayBuilder().apply(hexSeq"80", "", CborValue(1.5f), new IdentityParser[Any, CborValue])
			}
		}
		it ("Appends Double") {
			assertResult(Right(hexSeq"81 fbc010666666666666")){
				new CborArrayBuilder().apply(hexSeq"80", "", CborValue(-4.1D), new IdentityParser[Any, CborValue])
			}
		}
		it ("Appends empty string") {
			assertResult(Right(hexSeq"8160")){
				new CborArrayBuilder().apply(hexSeq"80", "", CborValue(""), new IdentityParser[Any, CborValue])
			}
		}
		it ("Appends \"abc\"") {
			assertResult(Right(hexSeq"8163616263")){
				new CborArrayBuilder().apply(hexSeq"80", "", CborValue("abc"), new IdentityParser[Any, CborValue])
			}
		}
		it ("Appends string containing multibyte char") {
			assertResult(Right(hexSeq"8163e6b0b4")){
				new CborArrayBuilder().apply(hexSeq"80", "", CborValue("\u6c34"), new IdentityParser[Any, CborValue])
			}
		}
		it ("Appends an empty byte array") {
			assertResult(Right(hexSeq"8140")){
				new CborArrayBuilder().apply(hexSeq"80", "", CborValue(new Array[Byte](0)), new IdentityParser[Any, CborValue])
			}
		}
		it ("Appends a non-empty byte array") {
			assertResult(Right(hexSeq"8143010203")){
				new CborArrayBuilder().apply(hexSeq"80", "", CborValue(hexArray"010203"), new IdentityParser[Any, CborValue])
			}
		}
		it ("Appends a second value") {
			assertResult(Right(hexSeq"""83616161626164""")){
				new CborArrayBuilder().apply(hexSeq"""8261616162""", "c", CborValue("d"), new IdentityParser[Any, CborValue])
			}
		}
		it ("Appends a 24th value") {
			assertResult(Right(hexSeq"""9818 B7A8B7A8B7A8 60""")){
				new CborArrayBuilder().apply(hexSeq"""97 B7A8B7A8B7A8 """, "", CborValue(""), new IdentityParser[Any, CborValue])
			}
		}
		it ("Appends a 101th value") {
			assertResult(Right(hexSeq"""9903e9 B7A8B7A8B7A8 60""")){
				new CborArrayBuilder().apply(hexSeq"""9903e8 B7A8B7A8B7A8 """, "", CborValue(""), new IdentityParser[Any, CborValue])
			}
		}
		it ("Appends a 256th value") {
			assertResult(Right(hexSeq"""990100 B7A8B7A8B7A8 60""")){
				new CborArrayBuilder().apply(hexSeq"""98FF B7A8B7A8B7A8 """, "", CborValue(""), new IdentityParser[Any, CborValue])
			}
		}
		it ("Appends a 65536th value") {
			assertResult(Right(hexSeq"""9A00010000 B7A8B7A8B7A8 60""")){
				new CborArrayBuilder().apply(hexSeq"""99FFFF B7A8B7A8B7A8 """, "", CborValue(""), new IdentityParser[Any, CborValue])
			}
		}
		it ("Appends a 1000001th value") {
			assertResult(Right(hexSeq"""9a000f4241 B7A8B7A8B7A8 60""")){
				new CborArrayBuilder().apply(hexSeq"""9a000f4240 B7A8B7A8B7A8 """, "", CborValue(""), new IdentityParser[Any, CborValue])
			}
		}
		
		it ("When parser reports a failure, the failure is forwarded") {
			assertResult( Left("FailureParser", 0) ){
				new CborArrayBuilder().apply(hexSeq"80", "", "", new FailureParser)
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
				).fold({x => x}, {x => x}, {(s,i) => ((s,i))})
			}
		}
		it ("CborArrayBuilder + JsonParser + nested arrays") {
			assertResult(hexSeq"81 82 00 01"){
				new JsonParser().parse(
					new CborArrayBuilder().mapKey[StringOrInt].mapValue[JsonValue],
					"""[[0,1]]"""
				).fold({x => x}, {x => x}, {(s,i) => ((s,i))})
			}
		}
		it ("CborArrayBuilder + CborParser + primitives") {
			assertResult(hexSeq"8262202005"){
				new CborParser().parse(
					new CborArrayBuilder().mapKey[CborValue].mapValue[CborValue],
					byteArray2DataInput(
							hexArray"8262202005"
					)
				).fold({x => x}, {x => x}, {(s,i) => ((s,i))})
			}
		}
	}
}
