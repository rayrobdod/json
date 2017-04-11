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
import com.rayrobdod.json.testing.EnforcedFailure
import com.rayrobdod.json.union.StringOrInt
import com.rayrobdod.json.union.JsonValue
import com.rayrobdod.json.union.CborValue
import com.rayrobdod.json.union.CborValue._
import com.rayrobdod.json.union.ParserRetVal.{Complex, BuilderFailure, ParserFailure}
import com.rayrobdod.json.parser.Parser
import com.rayrobdod.json.parser.PrimitiveSeqParser
import com.rayrobdod.json.parser.IdentityParser
import com.rayrobdod.json.parser.FailureParser
import com.rayrobdod.json.testing.HexArrayStringConverter
import scala.language.implicitConversions

class CborBuilderTest extends FunSpec {
	private implicit def integer2CborValue(x:Int):CborValue = CborValue(x)
	
	
	describe("CborBuilder") {
		describe ("init") {
			it ("is an instance of Middle") {
				assert(new CborBuilder.Middle() == new CborBuilder().init)
			}
			it ("has the same `forceobject` as the builder's constructor") {
				assert(new CborBuilder.Middle(true) == new CborBuilder(true).init)
			}
		}
		describe ("apply") {
			describe ("can append to an empty Middle (array)") {
				def doThing[A](name:String, value:A, hexRepr:Seq[Byte], parser:Parser[CborValue, CborValue, Any, Unit, A]) {
					it (name) {
						val exp = new CborBuilder.Middle(false, 1, hexRepr :: Nil)
						val res = new CborBuilder().apply[A, Any, Unit](new CborBuilder().init, 0, value, parser, ())
						assert( Complex(exp) == res )
					}
				}
				
				doThing("null", CborValueNull, hexSeq"F6", new IdentityParser[CborValue])
				doThing("true", CborValue(true), hexSeq"F5", new IdentityParser[CborValue])
				doThing("false", CborValue(false), hexSeq"F4", new IdentityParser[CborValue])
				doThing("0 as one byte", CborValue(0), hexSeq"00", new IdentityParser[CborValue])
				doThing("small posint as one byte", CborValue(6), hexSeq"06", new IdentityParser[CborValue])
				doThing("23 as one byte", CborValue(23), hexSeq"17", new IdentityParser[CborValue])
				doThing("24 as two bytes", CborValue(24), hexSeq"1818", new IdentityParser[CborValue])
				doThing("255 as two bytes", CborValue(255), hexSeq"18FF", new IdentityParser[CborValue])
				doThing("256 as three bytes", CborValue(256), hexSeq"190100", new IdentityParser[CborValue])
				doThing("1001 as three bytes", CborValue(1001), hexSeq"1903E9", new IdentityParser[CborValue])
				doThing("65535 as three bytes", CborValue(65535), hexSeq"19FFFF", new IdentityParser[CborValue])
				doThing("65536 as four bytes", CborValue(65536), hexSeq"1A00010000", new IdentityParser[CborValue])
				doThing("1000001", CborValue(1000001), hexSeq"1A000F4241", new IdentityParser[CborValue])
				doThing("1000000000000", CborValue(1000000000000L), hexSeq"1b000000e8d4a51000", new IdentityParser[CborValue])
				
				doThing("-1 as one byte", CborValue(-1), hexSeq"20", new IdentityParser[CborValue])
				doThing("-100 as two bytes", CborValue(-100), hexSeq"3863", new IdentityParser[CborValue])
				doThing("2**64 as BigInt", CborValue(BigInt(2).pow(64)), hexSeq"C249010000000000000000", new IdentityParser[CborValue])
				doThing("-2**64 - 1 as BigInt", CborValue(-BigInt(2).pow(64) - 1), hexSeq"C349010000000000000000", new IdentityParser[CborValue])
				doThing("Float", CborValue(1.5f), hexSeq"FA3FC00000", new IdentityParser[CborValue])
				doThing("Double", CborValue(-4.1D), hexSeq"fbc010666666666666", new IdentityParser[CborValue])
				doThing("BigDecimal", CborValue(BigDecimal("1.1")), hexSeq"C4 82 20 0B", new IdentityParser[CborValue])
				doThing("Rational", CborValue(new Rational(1,3)), hexSeq"D81E 82 01 03", new IdentityParser[CborValue])
				doThing("empty string", CborValue(""), hexSeq"60", new IdentityParser[CborValue])
				doThing("\"abc\"", CborValue("abc"), hexSeq"63616263", new IdentityParser[CborValue])
				doThing("string containing multibyte char", CborValue("\u6c34"), hexSeq"63e6b0b4", new IdentityParser[CborValue])
				doThing("an empty byte array", CborValue(new Array[Byte](0)), hexSeq"40", new IdentityParser[CborValue])
				doThing("a non-empty byte array", CborValue(hexArray"010203"), hexSeq"43010203", new IdentityParser[CborValue])
				
				doThing("an empty sequence", Seq.empty[CborValue], hexSeq"80", PrimitiveSeqParser[CborValue, CborValue, CborValue])
				doThing("a sequence of ints", Seq[CborValue](1,2,3), hexSeq"83010203", PrimitiveSeqParser[CborValue, CborValue, CborValue])
			}
			it ("Appends a second value to an array") {
				val exp = new CborBuilder.Middle(false, 2, hexSeq"6164" :: hexSeq"6161616" :: Nil)
				val input = new CborBuilder.Middle(false, 1, hexSeq"6161616" :: Nil)
				val res = new CborBuilder().apply(input, 1, CborValue("d"), new IdentityParser[CborValue], ())
				assert( Complex(exp) == res )
			}
			it ("Appends a 24th value") {
				val exp = new CborBuilder.Middle(false, 24, hexSeq"6164" :: hexSeq"TH3 R357" :: Nil)
				val input = new CborBuilder.Middle(false, 23, hexSeq"TH3 R357" :: Nil)
				val res = new CborBuilder().apply(input, 23, CborValue("d"), new IdentityParser[CborValue], ())
				assert( Complex(exp) == res )
			}
			
			
			it ("Appends null to empty object") {
				val exp = new CborBuilder.Middle(true, 1, hexSeq"00 F6" :: Nil)
				val input = new CborBuilder.Middle(true, 0, Nil)
				val res = new CborBuilder().apply(input, 0, CborValueNull, new IdentityParser[CborValue], ())
				assert( Complex(exp) == res )
			}
			it ("Converts an empty array into an object when given a non-zero key (is string)") {
				val exp = new CborBuilder.Middle(true, 1, hexSeq"60 F6" :: Nil)
				val input = new CborBuilder.Middle(false, 0, Nil)
				val res = new CborBuilder().apply(input, "", CborValueNull, new IdentityParser[CborValue], ())
				assert( Complex(exp) == res )
			}
			it ("Converts an empty array into an object when given a non-zero key (is int)") {
				val exp = new CborBuilder.Middle(true, 1, hexSeq"06 F6" :: Nil)
				val input = new CborBuilder.Middle(false, 0, Nil)
				val res = new CborBuilder().apply(input, 6, CborValueNull, new IdentityParser[CborValue], ())
				assert( Complex(exp) == res )
			}
			it ("Converts a two-element array into an object when given a non-2 key") {
				val exp = new CborBuilder.Middle(true, 3, hexSeq"05 6164" :: hexSeq"01 6162" :: hexSeq"00 6161" :: Nil)
				val input = new CborBuilder.Middle(false, 2, hexSeq"6162" :: hexSeq"6161" :: Nil)
				val res = new CborBuilder().apply(input, 5, CborValue("d"), new IdentityParser[CborValue], ())
				assert( Complex(exp) == res )
			}
			it ("Appends a second value to an object (aka does not double convert an object)") {
				val exp = new CborBuilder.Middle(true, 2, hexSeq"61616161" :: hexSeq"61626162" :: Nil)
				val input = new CborBuilder.Middle(true, 1, hexSeq"61626162" :: Nil)
				val res = new CborBuilder().apply(input, "a", CborValue("a"), new IdentityParser[CborValue], ())
				assert( Complex(exp) == res )
			}
			
			it ("When parser repcorts a failure, the failure is forwarded") {
				val exp = ParserFailure(EnforcedFailure)
				val input = new CborBuilder().init
				val res = new CborBuilder().apply(input, "", "", new FailureParser(EnforcedFailure), ())
				assert( exp == res )
			}
		}
		describe ("finish") {
			describe ("Uses the correct form when encoding the array length") {
				def doThing(count:Int, repr:Seq[Byte]) = {
					it (s"$count") {
						val exp = repr ++ hexSeq"CAFEBABE"
						val input = new CborBuilder.Middle(false, count, hexSeq"BABE" :: hexSeq"CAFE" :: Nil)
						val res = new CborBuilder().finish(())(input)
						assert( Complex(exp) == res )
					}
				}
				
				doThing(0, hexSeq"80")
				doThing(1, hexSeq"81")
				doThing(2, hexSeq"82")
				doThing(23, hexSeq"97")
				doThing(24, hexSeq"9818")
				doThing(255, hexSeq"98FF")
				doThing(256, hexSeq"990100")
				doThing(1001, hexSeq"9903E9")
				doThing(65535, hexSeq"99FFFF")
				doThing(65536, hexSeq"9A00010000")
				doThing(1000001, hexSeq"9A000F4241")
			}
			describe ("Uses the correct form when encoding the object length") {
				def doThing(count:Int, repr:Seq[Byte]) = {
					it (s"$count") {
						val exp = repr ++ hexSeq"CAFEBABE"
						val input = new CborBuilder.Middle(true, count, hexSeq"BABE" :: hexSeq"CAFE" :: Nil)
						val res = new CborBuilder().finish(())(input)
						assert( Complex(exp) == res )
					}
				}
				
				doThing(0, hexSeq"A0")
				doThing(1, hexSeq"A1")
				doThing(2, hexSeq"A2")
				doThing(23, hexSeq"B7")
				doThing(24, hexSeq"B818")
				doThing(255, hexSeq"B8FF")
				doThing(256, hexSeq"B90100")
				doThing(1001, hexSeq"B903E9")
				doThing(65535, hexSeq"B9FFFF")
				doThing(65536, hexSeq"BA00010000")
				doThing(1000001, hexSeq"BA000F4241")
			}
		}
	}
	
	describe("CborBuilder integration") {
		import com.rayrobdod.json.parser.{JsonParser, CborParser, byteArray2DataInput}
		
		it ("CborBuilder + JsonParser + primitive (array)") {
			assertResult(Complex(hexSeq"83 183D 183E 183F")){
				new JsonParser().parse(
					new CborBuilder().mapKey[StringOrInt].mapValue[JsonValue],
					"""[61, 62, 63]"""
				)
			}
		}
		it ("CborBuilder + JsonParser + nested objects (array)") {
			assertResult(Complex(hexSeq"81 A2 616100 616201")){
				new JsonParser().parse(
					new CborBuilder().mapKey[StringOrInt].mapValue[JsonValue],
					"""[{"a":0,"b":1}]"""
				)
			}
		}
		it ("CborBuilder + JsonParser + nested arrays (array)") {
			assertResult(Complex(hexSeq"81 82 00 01")){
				new JsonParser().parse(
					new CborBuilder().mapKey[StringOrInt].mapValue[JsonValue],
					"""[[0,1]]"""
				)
			}
		}
		it ("CborBuilder + CborParser + primitives (array)") {
			assertResult(Complex(hexSeq"8262202005")){
				new CborParser().parse(
					new CborBuilder(),
					byteArray2DataInput(
							hexArray"8262202005"
					)
				)
			}
		}
		
		
		it ("CborBuilder + JsonParser + primitive (object)") {
			assertResult(Complex(hexSeq"A3 6161 183D 6162 183E 6163 183F")){
				new JsonParser().parse(
					new CborBuilder().mapKey[StringOrInt].mapValue[JsonValue],
					"""{"a":61,"b":62,"c":63}"""
				)
			}
		}
		it ("CborBuilder + JsonParser + nested objects (object)") {
			assertResult(Complex(hexSeq"A1 60 A2 616100 616201")){
				new JsonParser().parse(
					new CborBuilder().mapKey[StringOrInt].mapValue[JsonValue],
					"""{"":{"a":0,"b":1}}"""
				)
			}
		}
		it ("CborBuilder + CborParser + primitive key (object)") {
			assertResult(Complex(hexSeq"A10405")){
				new CborParser().parse(
					new CborBuilder().mapValue[CborValue],
					byteArray2DataInput(
							hexArray"A10405"
					)
				)
			}
		}
		it ("CborBuilder + CborParser + primitives (object)") {
			assertResult(Complex(hexSeq"A162202005")){
				new CborParser().parse(
					new CborBuilder().mapValue[CborValue],
					byteArray2DataInput(
							hexArray"A162202005"
					)
				)
			}
		}
		it ("CborBuilder + CaseClassParser (object)") {
			assertResult(Complex(hexSeq"A3 6161 05 6162 f4 6163 63737472")){
				val builder = new CborBuilder().mapKey[String]
				builder.apply(builder.init, "a", CborValue(5), new IdentityParser[CborValue], ()).complex.flatMap{a:builder.Middle =>
					builder.apply(a, "b", CborValue(false), new IdentityParser[CborValue], ()).complex.flatMap{b:builder.Middle =>
						builder.apply(b, "c", CborValue("str"), new IdentityParser[CborValue], ()).complex.flatMap{c:builder.Middle =>
							builder.finish(())(c)
						}
					}
				}
			}
		}
	}
}
