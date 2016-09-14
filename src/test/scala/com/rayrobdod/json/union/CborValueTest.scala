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
import com.rayrobdod.json.union.CborValue._

class CborValueTest extends FunSpec {
	
	describe("CborValue") {
		// string, double, integer, boolean, null
		val values = Seq(
			CborValueString(""), CborValueByteStr(new Array[Byte](2)), 
			CborValueNumber(1.5), CborValueNumber(42),
			CborValueBoolean(true), CborValueNull
		)
		val ToEitherFuns = Seq[CborValue => Either[(String, Int),Any]](
			{x => x.stringToEither{s => Right(s)}},
			{x => x.byteArrayToEither{s => Right(s)}},
			{x => x.numberToEither{s => Right(s)}},
			{x => x.integerToEither{s => Right(s)}},
			{x => x.booleanToEither{s => Right(s)}}
		)
		val names = Seq("stringToEither", "byteArrayToEither", "numberToEither", "integerToEither", "booleanToEither", "nullToEither")
		val foldResults = Seq(0, 1, 2, 2, 3, 4)
		
		for (
			(v, vi) <- values.zipWithIndex;
			(f, fi) <- ToEitherFuns.zipWithIndex
		) {
			val rightExpected = (vi == fi) || (vi == 3 && fi == 2)
			
			it (s"""${v}.${names(fi)}(Right.apply) is ${if (rightExpected) {"right"} else {"left"}}""") {
				assertResult(rightExpected){f(v).isRight}
			}
		}
		
		for ((v, vi) <- values.zipWithIndex) {
			it (s"""${v}.fold invokes the ${foldResults(vi)}th  function""") {
				assertResult(foldResults(vi)){v.fold({x => 0}, {x => 1}, {x => 2}, {x => 3}, {() => 4})}
			}
		}
	}
	
	describe("CborValueByteStr") {
		it ("""is equal to a similar CborValueByteStr""") {
			val a = CborValueByteStr(Array(1,2,3,4,5))
			val b = CborValueByteStr(Array(1,2,3,4,5))
			
			assert(a.hashCode == b.hashCode)
			assert(a == b)
		}
		it ("""is not equal to a dissimilar CborValueByteStr""") {
			val a = CborValueByteStr(Array(1,2,3,4,5))
			val b = CborValueByteStr(Array(6,7,8,9,10))
			
			assert(a.hashCode != b.hashCode)
			assert(a != b)
		}
		it ("""is not equal to a completely different class""") {
			val a = CborValueByteStr(Array(1,2,3,4,5))
			val b = "asdf"
			
			assert(a != b)
		}
		it ("""is not linked to the source array (input)""") {
			val exp = Array[Byte](1,2,3,4,5)
			val arr = java.util.Arrays.copyOf(exp, 5)
			val cbor = CborValueByteStr(arr)
			assert(java.util.Arrays.equals(arr, cbor.s))
			
			arr(2) = 42
			assert(! java.util.Arrays.equals(arr, cbor.s))
		}
		it ("""is not linked to the source array (output)""") {
			val cbor = CborValueByteStr(Array[Byte](1,2,3,4,5))
			val arr = cbor.s
			assert(java.util.Arrays.equals(arr, cbor.s))
			
			arr(2) = 42
			assert(! java.util.Arrays.equals(arr, cbor.s))
		}
		it ("""tostring""") {
			val cbor = CborValueByteStr(Array[Byte](1,2,3,4,5))
			assertResult("CborValueByteStr([1, 2, 3, 4, 5])"){cbor.toString}
		}
	}
	
	describe("CborValue$") {
		describe("Implicits") {
			it ("""StringOrInt.Left to CborValue""") {
				val res:CborValue = StringOrInt("abx")
				assertResult(CborValue("abx")){res}
			}
			it ("""StringOrInt.Right to CborValue""") {
				val res:CborValue = StringOrInt(234)
				assertResult(CborValue(234)){res}
			}
			it ("""JsonValue.String to CborValue""") {
				val res:CborValue = JsonValue("abx")
				assertResult(CborValue("abx")){res}
			}
			it ("""JsonValue.Number to CborValue""") {
				val res:CborValue = JsonValue(234)
				assertResult(CborValue(234)){res}
			}
			it ("""JsonValue.Boolean to CborValue""") {
				val res:CborValue = JsonValue(true)
				assertResult(CborValue(true)){res}
			}
			it ("""JsonValue.Null to CborValue""") {
				val res:CborValue = JsonValue.JsonValueNull
				assertResult(CborValueNull){res}
			}
		}
		it ("""unwrap null""") {
			assertResult(null){CborValue.unwrap(CborValueNull)}
		}
		it ("""unsafeWrap bytestr succeeds""") {
			assertResult(CborValue(Array[Byte](1,2,3,4))){CborValue.unsafeWrap(Array[Byte](1,2,3,4))}
		}
		it ("""unsafeWrap intstr fails""") {
			intercept[MatchError]{ CborValue.unsafeWrap(Array[Int](1,2,3,4)) }
		}
	}
}
