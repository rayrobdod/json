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
package com.rayrobdod.json.union

import org.scalatest.FunSpec
import com.rayrobdod.json.builder.PiecewiseBuilder.Failures
import com.rayrobdod.json.union.JsonValue._

@deprecated("shut up compiler; I will cover deprecated methods", "")
class JsonValueTest extends FunSpec {
	
	describe("JsonValue") {
		// string, double, integer, boolean, null
		val values = Seq(
			JsonValueString(""),
			JsonValueNumber(1.5),
			JsonValueNumber(42),
			JsonValueBoolean(true), JsonValueNull
		)
		val ToEitherFuns = Seq[JsonValue => Either[Failures,Any]](
			{x => x.stringToEither{s => Right(s)}},
			{x => x.numberToEither{s => Right(s)}},
			{x => x.integerToEither{s => Right(s)}},
			{x => x.booleanToEither{s => Right(s)}}
		)
		val ifisFuns = Seq[JsonValue => Boolean](
			{x => x.ifIsString({t => true}, {e => false})},
			{x => x.ifIsNumber({t => true}, {e => false})},
			{x => x.ifIsInteger({t => true}, {e => false})},
			{x => x.ifIsBoolean({t => true}, {e => false})}
		)
		val names = Seq("stringToEither", "numberToEither", "integerToEither", "booleanToEither", "nullToEither")
		val names2 = Seq("ifIsString", "ifIsNumber", "ifIsInteger", "ifIsBoolean", "ifIsNull")
		val foldResults = Seq(0, 1, 1, 2, 3)
		
		for (
			(v, vi) <- values.zipWithIndex;
			(f, fi) <- ToEitherFuns.zipWithIndex
		) {
			val rightExpected = (vi == fi) || (vi == 2 && fi == 1)
			
			it (s"""${v}.${names(fi)}(Right.apply) is ${if (rightExpected) {"right"} else {"left"}}""") {
				assertResult(rightExpected){f(v).isRight}
			}
		}
		
		for (
			(v, vi) <- values.zipWithIndex;
			(f, fi) <- ifisFuns.zipWithIndex
		) {
			val rightExpected = (vi == fi) || (vi == 2 && fi == 1)
			
			it (s"""${v}.${names2(fi)} calls the ${if (rightExpected) {"first"} else {"second"}} function""") {
				assertResult(rightExpected){f(v)}
			}
		}
		
		for ((v, vi) <- values.zipWithIndex) {
			it (s"""${v}.fold invokes the ${foldResults(vi)}th  function""") {
				assertResult(foldResults(vi)){v.fold({x => 0}, {x => 1}, {x => 2}, {() => 3})}
			}
		}
	}
	
	describe("JsonValue$") {
		describe("Implicits") {
			it ("""StringOrInt.Left to JsonValue""") {
				val res:JsonValue = StringOrInt("abx")
				assertResult(JsonValue("abx")){res}
			}
			it ("""StringOrInt.Right to JsonValue""") {
				val res:JsonValue = StringOrInt(234)
				assertResult(JsonValue(234)){res}
			}
			it ("""String to JsonValue""") {
				val res:JsonValue = "abc"
				assertResult(JsonValueString("abc")){res}
			}
			it ("""Boolean to JsonValue""") {
				val res:JsonValue = true
				assertResult(JsonValueBoolean(true)){res}
			}
			it ("""Int to JsonValue""") {
				val res:JsonValue = 234
				assertResult(JsonValueNumber(234)){res}
			}
			it ("""Long to JsonValue""") {
				val res:JsonValue = 234L
				assertResult(JsonValueNumber(234)){res}
			}
		}
		describe("JsonValueNumber$.apply can accept") {
			val exp = JsonValueNumber(scala.math.BigDecimal("0"))
			
			it ("Int") {assertResult(exp){JsonValueNumber(0)}}
			it ("Long") {assertResult(exp){JsonValueNumber(0L)}}
			it ("Float") {assertResult(exp){JsonValueNumber(0F)}}
			it ("Double") {assertResult(exp){JsonValueNumber(0D)}}
			it ("BigDecimal") {assertResult(exp){JsonValueNumber(scala.math.BigDecimal("0"))}}
		}
		describe("JsonValue$.apply can accept") {
			val exp = JsonValueNumber(scala.math.BigDecimal("0"))
			
			it ("Int") {assertResult(exp){JsonValue(0)}}
			it ("Long") {assertResult(exp){JsonValue(0L)}}
			it ("Float") {assertResult(exp){JsonValue(0F)}}
			it ("Double") {assertResult(exp){JsonValue(0D)}}
			it ("BigDecimal") {assertResult(exp){JsonValue(scala.math.BigDecimal("0"))}}
		}
		describe("cborValueHexencodeByteStr") {
			it ("String") {assertResult(JsonValue("abc")){cborValueHexencodeByteStr(CborValue("abc"))}}
			it ("Number") {assertResult(JsonValue(123)){cborValueHexencodeByteStr(CborValue(123))}}
			it ("Boolean") {assertResult(JsonValue(true)){cborValueHexencodeByteStr(CborValue(true))}}
			it ("null") {assertResult(JsonValueNull){cborValueHexencodeByteStr(CborValue.CborValueNull)}}
			it ("bytestr") {assertResult(JsonValue("112345")){cborValueHexencodeByteStr(Array[Byte](17, 35, 69))}}
			it ("bytestr (with zeros)") {assertResult(JsonValue("01000010")){cborValueHexencodeByteStr(Array[Byte](1, 0, 0, 16))}}
		}
		describe("cborValue2jsonValue") {
			it ("String") {assertResult(Right(JsonValue("abc"))){cborValue2JsonValueEither(CborValue("abc"))}}
			it ("Number (rat)") {assertResult(Right(JsonValue(123))){cborValue2JsonValueEither(CborValue(123))}}
			it ("Number (unrat)") {assertResult(Left(Right(CborValue.Rational.NaN))){cborValue2JsonValueEither(CborValue(Double.NaN))}}
			it ("Boolean") {assertResult(Right(JsonValue(true))){cborValue2JsonValueEither(CborValue(true))}}
			it ("null") {assertResult(Right(JsonValueNull)){cborValue2JsonValueEither(CborValue.CborValueNull)}}
			it ("bytestr") {
				val src = Array[Byte](17, 35, 69)
				val res = cborValue2JsonValueEither(src)
				val res2 = res.left.get.left.get
				assert(src.sameElements(res2))
			}
		}
	}
}
