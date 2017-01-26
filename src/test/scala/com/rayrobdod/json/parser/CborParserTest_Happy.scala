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

import scala.collection.immutable.Map;
import org.scalatest.FunSpec;
import com.rayrobdod.json.builder.MapBuilder;
import com.rayrobdod.json.union.CborValue
import com.rayrobdod.json.union.CborValue._
import com.rayrobdod.json.testing.HexArrayStringConverter

final class CborParserTest_Happy extends FunSpec {
	
	private val testValues:Seq[(String, Array[Byte], CborParser.ParseReturnValue[Map[CborValue,Either[Nothing,CborValue]]], CborParser.TagMatcher)] = Seq(
		("false", Array[Byte](0xF4.byteValue), CborParser.ParseReturnValueSimple(CborValueBoolean(false)), CborParser.TagMatcher.empty),
		("true", Array[Byte](0xF5.byteValue), CborParser.ParseReturnValueSimple(CborValueBoolean(true)), CborParser.TagMatcher.empty),
		("null", Array[Byte](0xF6.byteValue), CborParser.ParseReturnValueSimple(CborValueNull), CborParser.TagMatcher.empty),
		("unknown", Array[Byte](0xE4.byteValue), CborParser.ParseReturnValueUnknownSimple(4), CborParser.TagMatcher.empty),
		("unknown (+byte)", hexArray"F842", CborParser.ParseReturnValueUnknownSimple(0x42), CborParser.TagMatcher.empty),
		("endOfObject", Array[Byte](0xFF.byteValue), CborParser.ParseReturnValueEndOfIndeterminateObject(), CborParser.TagMatcher.empty),
		("integer 0", Array[Byte](0), CborParser.ParseReturnValueSimple(CborValueNumber(new Rational(0, 1))), CborParser.TagMatcher.empty),
		("integer 1", Array[Byte](1), CborParser.ParseReturnValueSimple(CborValueNumber(1)), CborParser.TagMatcher.empty),
		("integer 15", Array[Byte](15), CborParser.ParseReturnValueSimple(CborValueNumber(15)), CborParser.TagMatcher.empty),
		("integer 23", Array[Byte](23), CborParser.ParseReturnValueSimple(CborValueNumber(23)), CborParser.TagMatcher.empty),
		("integer 0x12", Array[Byte](24, 0x12), CborParser.ParseReturnValueSimple(CborValueNumber(0x12)), CborParser.TagMatcher.empty),
		("integer 0x1234", Array[Byte](25, 0x12, 0x34), CborParser.ParseReturnValueSimple(CborValueNumber(0x1234)), CborParser.TagMatcher.empty),
		("integer 0xFFFF", Array[Byte](25, -1, -1), CborParser.ParseReturnValueSimple(CborValueNumber(BigInt(2).pow(16) - 1)), CborParser.TagMatcher.empty),
		("integer 0x12345678", Array[Byte](26, 0x12, 0x34, 0x56, 0x78), CborParser.ParseReturnValueSimple(CborValueNumber(0x12345678)), CborParser.TagMatcher.empty),
		("integer 0xFFFFFFFF", Array[Byte](26, -1, -1, -1, -1), CborParser.ParseReturnValueSimple(CborValueNumber(BigInt(2).pow(32) - 1)), CborParser.TagMatcher.empty),
		("integer 0x1234567890ABCDEF", Array(27, 0x12, 0x34, 0x56, 0x78, 0x90, 0xAB, 0xCD, 0xEF).map{_.byteValue}, CborParser.ParseReturnValueSimple(CborValueNumber(0x1234567890ABCDEFL)), CborParser.TagMatcher.empty),
		// ("integer 0xFFFFFFFFFFFFFFFF", Array[Byte](27, -1, -1, -1, -1, -1, -1, -1, -1), CborParser.ParseReturnValueSimple(CborValueNumber(BigInt(2).pow(64) - 1)), CborParser.TagMatcher.empty),
		("integer 43", Array[Byte](24, 43), CborParser.ParseReturnValueSimple(CborValueNumber(43)), CborParser.TagMatcher.empty),
		("integer -1", Array[Byte](0x20), CborParser.ParseReturnValueSimple(CborValueNumber(-1)), CborParser.TagMatcher.empty),
		("integer -5", Array[Byte](0x24), CborParser.ParseReturnValueSimple(CborValueNumber(-5)), CborParser.TagMatcher.empty),
		("halffloat 1.5", hexArray"F93E00", CborParser.ParseReturnValueSimple(CborValueNumber(1.5f)), CborParser.TagMatcher.empty),
		("float 1.5", hexArray"FA3FC00000", CborParser.ParseReturnValueSimple(CborValueNumber(1.5f)), CborParser.TagMatcher.empty),
		("doublefloat -4.1", hexArray"fbc010666666666666", CborParser.ParseReturnValueSimple(CborValueNumber(-4.1)), CborParser.TagMatcher.empty),
		("byte string 0", Array[Byte](0x40), CborParser.ParseReturnValueSimple(CborValueByteStr(Array[Byte]())), CborParser.TagMatcher.empty),
		("byte string 4", Array[Byte](0x44, 1,2,3,4), CborParser.ParseReturnValueSimple(CborValueByteStr(Array[Byte](1,2,3,4))), CborParser.TagMatcher.empty),
		("byte string 30", Array[Byte](0x58, 30) ++ (1 to 30).map{_.byteValue}, CborParser.ParseReturnValueSimple(CborValueByteStr((1.byteValue to 30.byteValue).map{_.byteValue}.toArray)), CborParser.TagMatcher.empty),
		("byte string INDET", hexArray"5F44AABBCCDD43EEFF99FF", CborParser.ParseReturnValueSimple(CborValueByteStr(hexArray"AABBCCDDEEFF99")), CborParser.TagMatcher.empty),
		("char string 0", Array[Byte](0x60), CborParser.ParseReturnValueSimple(CborValueString("")), CborParser.TagMatcher.empty),
		("char string 5", Array(0x65, 'h', 'e', 'l', 'l', 'o').map{_.byteValue}, CborParser.ParseReturnValueSimple(CborValueString("hello")), CborParser.TagMatcher.empty),
		("char string multibyte char", hexArray"63e6b0b4", CborParser.ParseReturnValueSimple(CborValueString("\u6c34") ), CborParser.TagMatcher.empty),
		("char string INDET", Array(0x7F, 0x62, 'h', 'e', 0x63, 'l', 'l', 'o', 0xFF).map{_.byteValue}, CborParser.ParseReturnValueSimple(CborValueString("hello") ), CborParser.TagMatcher.empty),
		("array 0", hexArray"80", CborParser.ParseReturnValueComplex(Map()), CborParser.TagMatcher.empty),
		("array 1", hexArray"8121", CborParser.ParseReturnValueComplex(Map(CborValue(0) -> Right(CborValue(-2)))), CborParser.TagMatcher.empty),
		("array 4", hexArray"8400010203", CborParser.ParseReturnValueComplex(Map(CborValue(0) -> Right(CborValue(0)), CborValue(1) -> Right(CborValue(1)), CborValue(2) -> Right(CborValue(2)), CborValue(3) -> Right(CborValue(3)))), CborParser.TagMatcher.empty),
		("array INDET", hexArray"9F00010203FF", CborParser.ParseReturnValueComplex(Map(CborValue(0) -> Right(CborValue(0)), CborValue(1) -> Right(CborValue(1)), CborValue(2) -> Right(CborValue(2)), CborValue(3) -> Right(CborValue(3)))), CborParser.TagMatcher.empty),
		("object 0", hexArray"A0", CborParser.ParseReturnValueComplex(Map()), CborParser.TagMatcher.empty),
		("object 1", hexArray"A10405", CborParser.ParseReturnValueComplex(Map(CborValue(4) -> Right(CborValue(5)))), CborParser.TagMatcher.empty),
		("object 2", hexArray"A2600061651865", CborParser.ParseReturnValueComplex(Map(CborValue("") -> Right(CborValue(0)), CborValue("e") -> Right(CborValue(0x65)))), CborParser.TagMatcher.empty),
		("object INDET", hexArray"BF600061651865FF", CborParser.ParseReturnValueComplex(Map(CborValue("") -> Right(CborValue(0)), CborValue("e") -> Right(CborValue(0x65)))), CborParser.TagMatcher.empty),
		("tag self-describing (unknown)", hexArray"d9d9f7 80", CborParser.ParseReturnValueTaggedValue(55799, CborParser.ParseReturnValueComplex(Map())), CborParser.TagMatcher.empty)
		, ("tag self-describing (known)", hexArray"d9d9f7 80", CborParser.ParseReturnValueComplex(Map()), CborParser.TagMatcher.selfDescribe)
		, ("tag rational (known)", hexArray"d81e 820103", CborParser.ParseReturnValueSimple(CborValue(new Rational(1, 3))), CborParser.TagMatcher.numbers)
		, ("tag positive bigint (known)", hexArray"c2 43123456", CborParser.ParseReturnValueSimple(CborValue(0x123456)), CborParser.TagMatcher.numbers)
		, ("tag negative bigint (known)", hexArray"c3 43123456", CborParser.ParseReturnValueSimple(CborValue(-0x123457)), CborParser.TagMatcher.numbers)
		, ("tag decimal frac (known)", hexArray"c4 820301", CborParser.ParseReturnValueSimple(CborValue(1000)), CborParser.TagMatcher.numbers)
		, ("tag binary frac (known)", hexArray"c5 820301", CborParser.ParseReturnValueSimple(CborValue(8)), CborParser.TagMatcher.numbers)
		, ("tag binary frac (known) (2)", hexArray"c5 822301", CborParser.ParseReturnValueSimple(CborValue(1.0 / 16)), CborParser.TagMatcher.numbers)
	)
	
	
	describe("CborParser + MapBuilder can decode") {
		testValues.foreach{case (testName, sourceArray, expected, tagMatcher) =>
			it(testName) {
				val source = new java.io.DataInputStream(
					new java.io.ByteArrayInputStream(
						sourceArray
					)
				)
				val result = new CborParser(tagMatcher).parseDetailed(MapBuilder.apply, source)
				assertResult(expected){result}
			}
		}
	}
}
