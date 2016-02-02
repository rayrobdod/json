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
import com.rayrobdod.json.parser.HexArrayStringConverter;
import com.rayrobdod.json.union.StringOrInt.AsStringKeyBuilder

class CborArrayBuilderTest extends FunSpec {
	private case class Abc(a:Int, b:Boolean, c:String)
	private implicit def classAbc = classOf[Abc]
	
	
	describe("CborArrayBuilder") {
		it ("inits correctly") {
			assertResult(Seq(0x80.byteValue)){new CborArrayBuilder().init}
		}
		it ("Appends null") {
			assertResult(hexSeq"81f6"){
				new CborArrayBuilder().apply(hexSeq"80", "", null)
			}
		}
		it ("Appends true") {
			assertResult(hexSeq"81f5"){
				new CborArrayBuilder().apply(hexSeq"80", "", true)
			}
		}
		it ("Appends false") {
			assertResult(hexSeq"81f4"){
				new CborArrayBuilder().apply(hexSeq"80", "", false)
			}
		}
		it ("Appends 0 as one byte") {
			assertResult(hexSeq"8100"){
				new CborArrayBuilder().apply(hexSeq"80", "", 0)
			}
		}
		it ("Appends small positive integer") {
			assertResult(hexSeq"8106"){
				new CborArrayBuilder().apply(hexSeq"80", "", 6)
			}
		}
		it ("Appends 23 as one byte") {
			assertResult(hexSeq"8117"){
				new CborArrayBuilder().apply(hexSeq"80", "", 23)
			}
		}
		it ("Appends 24 as two bytes") {
			assertResult(hexSeq"811818"){
				new CborArrayBuilder().apply(hexSeq"80", "", 24)
			}
		}
		it ("Appends 255 as two bytes") {
			assertResult(hexSeq"8118FF"){
				new CborArrayBuilder().apply(hexSeq"80", "", 255)
			}
		}
		it ("Appends 256 as three bytes") {
			assertResult(hexSeq"81190100"){
				new CborArrayBuilder().apply(hexSeq"80", "", 256)
			}
		}
		it ("Appends -1 as one byte") {
			assertResult(hexSeq"8120"){
				new CborArrayBuilder().apply(hexSeq"80", "", -1)
			}
		}
		it ("Appends -100 as two bytes") {
			assertResult(hexSeq"813863"){
				new CborArrayBuilder().apply(hexSeq"80", "", -100)
			}
		}
		it ("Appends Float") {
			assertResult(hexArray"81 FA3FC00000"){
				new CborArrayBuilder().apply(hexSeq"80", "", 1.5f)
			}
		}
		it ("Appends Double") {
			assertResult(hexArray"81 fbc010666666666666"){
				new CborArrayBuilder().apply(hexSeq"80", "", -4.1D)
			}
		}
		it ("Appends empty string") {
			assertResult(hexSeq"8160"){
				new CborArrayBuilder().apply(hexSeq"80", "", "")
			}
		}
		it ("Appends \"abc\"") {
			assertResult(hexSeq"8163616263"){
				new CborArrayBuilder().apply(hexSeq"80", "", "abc")
			}
		}
		it ("Appends string containing multibyte char") {
			assertResult(hexSeq"8163e6b0b4"){
				new CborArrayBuilder().apply(hexSeq"80", "", "\u6c34")
			}
		}
		it ("Appends an empty byte array") {
			assertResult(hexSeq"8140"){
				new CborArrayBuilder().apply(hexSeq"80", "", new Array[Byte](0))
			}
		}
		it ("Appends a non-empty byte array") {
			assertResult(hexSeq"8143010203"){
				new CborArrayBuilder().apply(hexSeq"80", "", hexArray"010203")
			}
		}
		it ("Appends a second value") {
			assertResult(hexSeq"""83616161626164"""){
				new CborArrayBuilder().apply(hexSeq"""8261616162""", "c", "d")
			}
		}
		it ("Appends a 24th value") {
			assertResult(hexSeq"""9818 B7A8B7A8B7A8 60"""){
				new CborArrayBuilder().apply(hexSeq"""97 B7A8B7A8B7A8 """, "", "")
			}
		}
		it ("Appends a 101th value") {
			assertResult(hexSeq"""9903e9 B7A8B7A8B7A8 60"""){
				new CborArrayBuilder().apply(hexSeq"""9903e8 B7A8B7A8B7A8 """, "", "")
			}
		}
		it ("Appends a 256th value") {
			assertResult(hexSeq"""990100 B7A8B7A8B7A8 60"""){
				new CborArrayBuilder().apply(hexSeq"""98FF B7A8B7A8B7A8 """, "", "")
			}
		}
		it ("Appends a 65536th value") {
			assertResult(hexSeq"""9A00010000 B7A8B7A8B7A8 60"""){
				new CborArrayBuilder().apply(hexSeq"""99FFFF B7A8B7A8B7A8 """, "", "")
			}
		}
		it ("Appends a 1000001th value") {
			assertResult(hexSeq"""9a000f4241 B7A8B7A8B7A8 60"""){
				new CborArrayBuilder().apply(hexSeq"""9a000f4240 B7A8B7A8B7A8 """, "", "")
			}
		}
		it ("childBuilder returns MapBuilder") {
			val builder = new CborArrayBuilder()
			
			assert{builder.childBuilder("").isInstanceOf[MapBuilder[_]]}
		}
		it ("resultType returns constructor parameter `clazz`") {
			assertResult(classOf[Seq[Byte]]){
				new CborArrayBuilder().resultType
			}
		}
	}
	
	describe("CborArrayBuilder integration") {
		import com.rayrobdod.json.parser.{JsonParser, SeqParser, CborParser, CaseClassParser, byteArray2DataInput}
		
		it ("CborArrayBuilder + JsonParser + primitive") {
			assertResult(hexSeq"83 183D 183E 183F"){
				new JsonParser(new CborArrayBuilder).parse(
					"""[61, 62, 63]"""
				)
			}
		}
		it ("CborArrayBuilder + JsonParser + nested objects") {
			assertResult(hexSeq"81 A2 616100 616201"){
				new JsonParser(new AsStringKeyBuilder(new CborArrayBuilder)).parse(
					"""[{"a":0,"b":1}]"""
				)
			}
		}
		it ("CborArrayBuilder + CborParser + primitives") {
			assertResult(hexSeq"8262202005"){
				new CborParser(new CborArrayBuilder).parse(
					byteArray2DataInput(
							hexArray"8262202005"
					)
				)
			}
		}
		it ("CborArrayBuilder + SeqParser + nested arrays") {
			assertResult(hexSeq"81818105"){
				new SeqParser(new CborArrayBuilder).parse(
					Seq(Seq(Seq(5)))
				)
			}
		}
		it ("CborArrayBuilder + nested case classes") {
			val pf:PartialFunction[Any,Any] = {case x:Abc => new CaseClassParser(new MapBuilder).parse(x)}
			
			assertResult(hexSeq"81 A3 6161 05 6162 f4 6163 63737472"){
				new SeqParser(new CborArrayBuilder(transformer = pf)).parse(Seq(Abc(5,false,"str")))
			}
		}
	}
}
