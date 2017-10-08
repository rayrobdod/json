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
package com.rayrobdod.json.parser;

import org.scalatest.FunSpec;
import scala.collection.immutable.Seq
import com.rayrobdod.json.union.CborValue
import com.rayrobdod.json.union.CborValue.Rational
import com.rayrobdod.json.builder._
import com.rayrobdod.json.parser.CborParser._
import com.rayrobdod.json.testing.HexArrayStringConverter

class CborParserTagMatcherTest extends FunSpec {
	
	import scala.language.implicitConversions
	private implicit def arrayToDataInput(sourceArray:Array[Byte]):java.io.DataInput = {
		new java.io.DataInputStream(
			new java.io.ByteArrayInputStream(
				sourceArray
			)
		)
	}
	
	describe("CborParser.TagMatcher") {
		describe("selfDescribe") {
			describe("unapply(55799)") {
				it ("isDefined") {
					assert( CborParser.TagMatcher.selfDescribe.unapply(55799).isDefined )
				}
				it ("is the same as a raw parseDetailed") {
					val tagFun = CborParser.TagMatcher.selfDescribe.unapply(55799).get
					assertResult( new CborParser(CborParser.TagMatcher.empty).parseDetailed( MapBuilder.apply, hexArray"43123456" ) ){ tagFun.apply( MapBuilder.apply, hexArray"43123456" ) }
				}
			}
			it ("unapply returns none for any other value") {
				assert( !CborParser.TagMatcher.selfDescribe.unapply(2).isDefined )
			}
		}
		describe("numbers") {
			describe("unapply(2) pos bigint") {
				it ("isDefined") {
					assert( CborParser.TagMatcher.numbers.unapply(2).isDefined )
				}
				it ("happy when given a byte array") {
					val tagFun = CborParser.TagMatcher.numbers.unapply(2).get
					assertResult( ParseReturnValueSimple(CborValue(0x123456)) ){ tagFun.apply( MapBuilder.apply, hexArray"43123456" ) }
				}
				it ("happy when given a byte array (2 ** 64)") {
					val tagFun = CborParser.TagMatcher.numbers.unapply(2).get
					assertResult( ParseReturnValueSimple(CborValue(BigInt(2).pow(64))) ){ tagFun.apply( MapBuilder.apply, hexArray"49010000000000000000" ) }
				}
				it ("fails when given a posint") {
					val tagFun = CborParser.TagMatcher.numbers.unapply(2).get
					assert( tagFun.apply( MapBuilder.apply, hexArray"11" ).isInstanceOf[ParseReturnValueParserFailure] )
				}
				it ("fails when given an array") {
					val tagFun = CborParser.TagMatcher.numbers.unapply(2).get
					assert( tagFun.apply( MapBuilder.apply, hexArray"8400010203" ).isInstanceOf[ParseReturnValueParserFailure] )
				}
			}
			describe("unapply(3) neg bigint") {
				it ("isDefined") {
					assert( CborParser.TagMatcher.numbers.unapply(3).isDefined )
				}
				it ("happy when given a byte array") {
					val tagFun = CborParser.TagMatcher.numbers.unapply(3).get
					assertResult( ParseReturnValueSimple(CborValue(-0x123457)) ){ tagFun.apply( MapBuilder.apply, hexArray"43123456" ) }
				}
				it ("fails when given a posint") {
					val tagFun = CborParser.TagMatcher.numbers.unapply(3).get
					assert( tagFun.apply( MapBuilder.apply, hexArray"11" ).isInstanceOf[ParseReturnValueParserFailure] )
				}
				it ("fails when given an array") {
					val tagFun = CborParser.TagMatcher.numbers.unapply(3).get
					assert( tagFun.apply( MapBuilder.apply, hexArray"8400010203" ).isInstanceOf[ParseReturnValueParserFailure] )
				}
			}
			describe("unapply(4) decimal bigfloat") {
				it ("isDefined") {
					assert( CborParser.TagMatcher.numbers.unapply(4).isDefined )
				}
				it ("happy when given a two-element array") {
					val tagFun = CborParser.TagMatcher.numbers.unapply(4).get
					assertResult( ParseReturnValueSimple(CborValue(2000)) ){ tagFun.apply( MapBuilder.apply, hexArray"820302" ) }
				}
				it ("happy when given a two-element array (273.15)") {
					val tagFun = CborParser.TagMatcher.numbers.unapply(4).get
					assertResult( ParseReturnValueSimple(CborValue(BigDecimal("273.15"))) ){ tagFun.apply( MapBuilder.apply, hexArray"82 21 196ab3" ) }
				}
				it ("fails when given a posint") {
					val tagFun = CborParser.TagMatcher.numbers.unapply(4).get
					assert( tagFun.apply( MapBuilder.apply, hexArray"11" ).isInstanceOf[ParseReturnValueParserFailure] )
				}
				ignore ("fails when given a single-element array") {
					val tagFun = CborParser.TagMatcher.numbers.unapply(4).get
					assert( tagFun.apply( MapBuilder.apply, hexArray"8104" ).isInstanceOf[ParseReturnValueParserFailure] )
				}
				it ("fails when given a three-element array") {
					val tagFun = CborParser.TagMatcher.numbers.unapply(4).get
					assert( tagFun.apply( MapBuilder.apply, hexArray"83010203" ).isInstanceOf[ParseReturnValueParserFailure] )
				}
				it ("fails when given a string array") {
					val tagFun = CborParser.TagMatcher.numbers.unapply(4).get
					assert( tagFun.apply( MapBuilder.apply, hexArray"824040" ).isInstanceOf[ParseReturnValueParserFailure] )
				}
				it ("fails when given a float array") {
					val tagFun = CborParser.TagMatcher.numbers.unapply(4).get
					assert( tagFun.apply( MapBuilder.apply, hexArray"82 FA3FC00000 FA3FC00000" ).isInstanceOf[ParseReturnValueParserFailure] )
				}
				it ("fails when given a map") {
					val tagFun = CborParser.TagMatcher.numbers.unapply(4).get
					assert( tagFun.apply( MapBuilder.apply, hexArray"A2 4121 01 4222 02" ).isInstanceOf[ParseReturnValueParserFailure] )
				}
			}
			describe("unapply(5) binary bigfloat") {
				it ("isDefined") {
					assert( CborParser.TagMatcher.numbers.unapply(5).isDefined )
				}
				it ("happy when given a two-element array") {
					val tagFun = CborParser.TagMatcher.numbers.unapply(5).get
					assertResult( ParseReturnValueSimple(CborValue(16)) ){ tagFun.apply( MapBuilder.apply, hexArray"820302" ) }
				}
				it ("happy when given a two-element array (1.5)") {
					val tagFun = CborParser.TagMatcher.numbers.unapply(5).get
					assertResult( ParseReturnValueSimple(CborValue(BigDecimal("1.5"))) ){ tagFun.apply( MapBuilder.apply, hexArray"82 20 03" ) }
				}
				it ("fails when given a posint") {
					val tagFun = CborParser.TagMatcher.numbers.unapply(5).get
					assert( tagFun.apply( MapBuilder.apply, hexArray"11" ).isInstanceOf[ParseReturnValueParserFailure] )
				}
				ignore ("fails when given a single-element array") {
					val tagFun = CborParser.TagMatcher.numbers.unapply(5).get
					assert( tagFun.apply( MapBuilder.apply, hexArray"8104" ).isInstanceOf[ParseReturnValueParserFailure] )
				}
				it ("fails when given a three-element array") {
					val tagFun = CborParser.TagMatcher.numbers.unapply(5).get
					assert( tagFun.apply( MapBuilder.apply, hexArray"83010203" ).isInstanceOf[ParseReturnValueParserFailure] )
				}
				it ("fails when given a string array") {
					val tagFun = CborParser.TagMatcher.numbers.unapply(5).get
					assert( tagFun.apply( MapBuilder.apply, hexArray"824040" ).isInstanceOf[ParseReturnValueParserFailure] )
				}
				it ("fails when given a float array") {
					val tagFun = CborParser.TagMatcher.numbers.unapply(5).get
					assert( tagFun.apply( MapBuilder.apply, hexArray"82 FA3FC00000 FA3FC00000" ).isInstanceOf[ParseReturnValueParserFailure] )
				}
				it ("fails when given a map") {
					val tagFun = CborParser.TagMatcher.numbers.unapply(5).get
					assert( tagFun.apply( MapBuilder.apply, hexArray"A2 4121 01 4222 02" ).isInstanceOf[ParseReturnValueParserFailure] )
				}
			}
			describe("unapply(30) decimal bigfloat") {
				it ("isDefined") {
					assert( CborParser.TagMatcher.numbers.unapply(30).isDefined )
				}
				it ("happy when given a two-element array") {
					val tagFun = CborParser.TagMatcher.numbers.unapply(30).get
					assertResult( ParseReturnValueSimple(CborValue(new Rational(3,2))) ){ tagFun.apply( MapBuilder.apply, hexArray"820302" ) }
				}
				it ("happy when given a two-element array (-1/3)") {
					val tagFun = CborParser.TagMatcher.numbers.unapply(30).get
					assertResult( ParseReturnValueSimple(CborValue(new Rational(-1, 3))) ){ tagFun.apply( MapBuilder.apply, hexArray"82 20 03" ) }
				}
				it ("fails when given a posint") {
					val tagFun = CborParser.TagMatcher.numbers.unapply(30).get
					assert( tagFun.apply( MapBuilder.apply, hexArray"11" ).isInstanceOf[ParseReturnValueParserFailure] )
				}
				ignore ("fails when given a single-element array") {
					val tagFun = CborParser.TagMatcher.numbers.unapply(30).get
					assert( tagFun.apply( MapBuilder.apply, hexArray"8104" ).isInstanceOf[ParseReturnValueParserFailure] )
				}
				it ("fails when given a three-element array") {
					val tagFun = CborParser.TagMatcher.numbers.unapply(30).get
					assert( tagFun.apply( MapBuilder.apply, hexArray"83010203" ).isInstanceOf[ParseReturnValueParserFailure] )
				}
				it ("fails when given a string array") {
					val tagFun = CborParser.TagMatcher.numbers.unapply(30).get
					assert( tagFun.apply( MapBuilder.apply, hexArray"824040" ).isInstanceOf[ParseReturnValueParserFailure] )
				}
				it ("fails when given a float array") {
					val tagFun = CborParser.TagMatcher.numbers.unapply(30).get
					assert( tagFun.apply( MapBuilder.apply, hexArray"82 FA3FC00000 FA3FC00000" ).isInstanceOf[ParseReturnValueParserFailure] )
				}
				it ("fails when given a map") {
					val tagFun = CborParser.TagMatcher.numbers.unapply(30).get
					assert( tagFun.apply( MapBuilder.apply, hexArray"A2 4121 01 4222 02" ).isInstanceOf[ParseReturnValueParserFailure] )
				}
			}
			it ("unapply returns none for any other value") {
				assert( !CborParser.TagMatcher.numbers.unapply(100000).isDefined )
			}
		}
		describe("numbers.orElse(selfDescribe)") {
			import CborParser.TagMatcher._
			val matcher = numbers.orElse(selfDescribe)
			
			Seq(2,3,4,5,30).foreach{x =>
				it (s"is defined at $x") {assert( matcher.unapply(x).isDefined )}
				// it (s"returns the same value as numbers at $x") { assertResult( numbers.unapply(x) ){ matcher.unapply(x) } }
			}
			Seq(55799).foreach{x =>
				it (s"is defined at $x") {assert( matcher.unapply(x).isDefined )}
				// it (s"returns the same value as selfDescribe at $x") { assertResult( selfDescribe.unapply(x) ){ matcher.unapply(x) } }
			}
			it (s"not isDefined at other values") { assert( ! matcher.unapply(1000000).isDefined ) }
		}
	}
}
