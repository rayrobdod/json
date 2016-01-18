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

import java.text.ParseException;
import scala.collection.immutable.{Seq, Map};
import org.scalatest.FunSpec;
import java.nio.charset.StandardCharsets.US_ASCII;
import com.rayrobdod.json.parser.{byteArray2DataInput, HexArrayStringConverter}

class CborObjectBuilderTest extends FunSpec {
	describe("CborObjectBuilder") {
		it ("inits correctly") {
			assertResult(Seq(0xA0.byteValue)){new CborObjectBuilder().init}
		}
		it ("Appends null") {
			assertResult(hexSeq"A160f6"){
				new CborObjectBuilder().apply(hexSeq"A0", "", null)
			}
		}
		it ("Appends true") {
			assertResult(hexSeq"A160f5"){
				new CborObjectBuilder().apply(hexSeq"A0", "", true)
			}
		}
		it ("Appends false") {
			assertResult(hexSeq"A160f4"){
				new CborObjectBuilder().apply(hexSeq"A0", "", false)
			}
		}
		it ("Appends 0 as one byte") {
			assertResult(hexSeq"A16000"){
				new CborObjectBuilder().apply(hexSeq"A0", "", 0)
			}
		}
		it ("Appends small positive integer") {
			assertResult(hexSeq"A16006"){
				new CborObjectBuilder().apply(hexSeq"A0", "", 6)
			}
		}
		it ("Appends 23 as one byte") {
			assertResult(hexSeq"A16017"){
				new CborObjectBuilder().apply(hexSeq"A0", "", 23)
			}
		}
		it ("Appends 24 as two bytes") {
			assertResult(hexSeq"A1601818"){
				new CborObjectBuilder().apply(hexSeq"A0", "", 24)
			}
		}
		it ("Appends 255 as two bytes") {
			assertResult(hexSeq"A16018FF"){
				new CborObjectBuilder().apply(hexSeq"A0", "", 255)
			}
		}
		it ("Appends 256 as three bytes") {
			assertResult(hexSeq"A160190100"){
				new CborObjectBuilder().apply(hexSeq"A0", "", 256)
			}
		}
		it ("Appends -1 as one byte") {
			assertResult(hexSeq"A16020"){
				new CborObjectBuilder().apply(hexSeq"A0", "", -1)
			}
		}
		it ("Appends -100 as two bytes") {
			assertResult(hexSeq"A1603863"){
				new CborObjectBuilder().apply(hexSeq"A0", "", -100)
			}
		}
		it ("Appends Float") {
			assertResult(hexArray"A160 FA3FC00000"){
				new CborObjectBuilder().apply(hexSeq"A0", "", 1.5f)
			}
		}
		it ("Appends Double") {
			assertResult(hexArray"A160 fbc010666666666666"){
				new CborObjectBuilder().apply(hexSeq"A0", "", -4.1D)
			}
		}
		it ("Appends empty string") {
			assertResult(hexSeq"A16060"){
				new CborObjectBuilder().apply(hexSeq"A0", "", "")
			}
		}
		it ("Appends \"abc\"") {
			assertResult(hexSeq"A16063616263"){
				new CborObjectBuilder().apply(hexSeq"A0", "", "abc")
			}
		}
		it ("Appends string containing multibyte char") {
			assertResult(hexSeq"A16063e6b0b4"){
				new CborObjectBuilder().apply(hexSeq"A0", "", "\u6c34")
			}
		}
		it ("Appends an empty byte array") {
			assertResult(hexSeq"A16040"){
				new CborObjectBuilder().apply(hexSeq"A0", "", new Array[Byte](0))
			}
		}
		it ("Appends a non-empty byte array") {
			assertResult(hexSeq"A16043010203"){
				new CborObjectBuilder().apply(hexSeq"A0", "", hexArray"010203")
			}
		}
		it ("Appends a second value") {
			assertResult(hexSeq"""A26161616261636164"""){
				new CborObjectBuilder().apply(hexSeq"""A161616162""", "c", "d")
			}
		}
		it ("Appends a 24th value") {
			assertResult(hexSeq"""B818 B7A8B7A8B7A8 6060"""){
				new CborObjectBuilder().apply(hexSeq"""B7 B7A8B7A8B7A8 """, "", "")
			}
		}
		it ("Appends a 101th value") {
			assertResult(hexSeq"""B903e9 B7A8B7A8B7A8 6060"""){
				new CborObjectBuilder().apply(hexSeq"""B903e8 B7A8B7A8B7A8 """, "", "")
			}
		}
		it ("Appends a 256th value") {
			assertResult(hexSeq"""B90100 B7A8B7A8B7A8 6060"""){
				new CborObjectBuilder().apply(hexSeq"""B8FF B7A8B7A8B7A8 """, "", "")
			}
		}
		it ("Appends a 65536th value") {
			assertResult(hexSeq"""BA00010000 B7A8B7A8B7A8 6060"""){
				new CborObjectBuilder().apply(hexSeq"""B9FFFF B7A8B7A8B7A8 """, "", "")
			}
		}
		it ("Appends a 1000001th value") {
			assertResult(hexSeq"""Ba000f4241 B7A8B7A8B7A8 6060"""){
				new CborObjectBuilder().apply(hexSeq"""Ba000f4240 B7A8B7A8B7A8 """, "", "")
			}
		}
		it ("childBuilder returns MapBuilder") {
			val builder = new CborObjectBuilder()
			
			assert{builder.childBuilder("").isInstanceOf[MapBuilder[_]]}
		}
		it ("resultType returns constructor parameter `clazz`") {
			assertResult(classOf[Seq[Byte]]){
				new CborObjectBuilder().resultType
			}
		}
	}
	
	describe("CborObjectBuilder integration") {
		import com.rayrobdod.json.parser.{JsonParser, CborParser}
		
		it ("CborObjectBuilder + JsonParser + primitive") {
			assertResult(hexSeq"A3 6161 183D 6162 183E 6163 183F"){
				new JsonParser(new ToStringKeyBuilder(new CborObjectBuilder)).parse(
					"""{"a":61,"b":62,"c":63}"""
				)
			}
		}
		it ("CborObjectBuilder + JsonParser + nested objects") {
			assertResult(hexSeq"A1 60 A2 616100 616201"){
				new JsonParser(new ToStringKeyBuilder(new CborObjectBuilder)).parse(
					"""{"":{"a":0,"b":1}}"""
				)
			}
		}
		it ("CborObjectBuilder + CborParser + primitive key") {
			assertResult(hexSeq"A1613405"){
				new CborParser(new CborObjectBuilder).parse(
					byteArray2DataInput(
							hexArray"A10405"
					)
				)
			}
		}
		it ("CborObjectBuilder + CborParser + primitives") {
			assertResult(hexSeq"A162202005"){
				new CborParser(new CborObjectBuilder).parse(
					byteArray2DataInput(
							hexArray"A162202005"
					)
				)
			}
		}
	}
}
