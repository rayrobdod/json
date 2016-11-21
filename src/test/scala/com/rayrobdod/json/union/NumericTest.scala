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
import com.rayrobdod.json.union.Numeric._
import java.math.MathContext.UNLIMITED

class NumericTest extends FunSpec {
	
	private val intMaxPlusOne = (Int.MaxValue:Long) + 1
	private val intMinMinusOne = (Int.MinValue:Long) - 1
	
	describe("IntNumeric") {
		val tests = Seq[(Int, Option[BigDecimal], Option[BigInt],
				Option[Double], Option[Float], Option[Long], Option[Int])](
			  (0, Option(BigDecimal(0)), Option(BigInt(0)), Option(0), Option(0), Option(0), Option(0))
			, (1, Option(BigDecimal(1)), Option(BigInt(1)), Option(1), Option(1), Option(1), Option(1))
			, (-1, Option(BigDecimal(-1)), Option(BigInt(-1)), Option(-1), Option(-1), Option(-1), Option(-1))
			, (Int.MaxValue, Option(BigDecimal(Int.MaxValue)), Option(BigInt(Int.MaxValue)), Option(Int.MaxValue), None, Option(Int.MaxValue), Option(Int.MaxValue))
			, (Int.MinValue, Option(BigDecimal(Int.MinValue)), Option(BigInt(Int.MinValue)), Option(Int.MinValue), Option(Int.MinValue), Option(Int.MinValue), Option(Int.MinValue))
		)
		
		tests.foreach{x =>
			describe("In Value: " + x._1) {
				it ("convert to BigDec: " + x._2) {assertResult(x._2){LongNumeric.tryToBigDecimal(x._1)}}
				it ("convert to BigInt: " + x._3) {assertResult(x._3){LongNumeric.tryToBigInt(x._1)}}
				it ("convert to Double: " + x._4) {assertResult(x._4){LongNumeric.tryToDouble(x._1)}}
				it ("convert to Float: " + x._5) {assertResult(x._5){LongNumeric.tryToFloat(x._1)}}
				it ("convert to Long: " + x._6) {assertResult(x._6){LongNumeric.tryToLong(x._1)}}
				it ("convert to Int: " + x._7) {assertResult(x._7){LongNumeric.tryToInt(x._1)}}
			}
		}
	}
	describe("LongNumeric") {
		val tests = Seq[(Long, Option[BigDecimal], Option[BigInt],
				Option[Double], Option[Float], Option[Long], Option[Int])](
			  (0, Option(BigDecimal(0)), Option(BigInt(0)), Option(0), Option(0), Option(0), Option(0))
			, (1, Option(BigDecimal(1)), Option(BigInt(1)), Option(1), Option(1), Option(1), Option(1))
			, (-1, Option(BigDecimal(-1)), Option(BigInt(-1)), Option(-1), Option(-1), Option(-1), Option(-1))
			, (Int.MaxValue, Option(BigDecimal(Int.MaxValue)), Option(BigInt(Int.MaxValue)), Option(Int.MaxValue), None, Option(Int.MaxValue), Option(Int.MaxValue))
			, (intMaxPlusOne, Option(BigDecimal(intMaxPlusOne)), Option(BigInt(intMaxPlusOne)), Option(intMaxPlusOne), Option(intMaxPlusOne), Option(intMaxPlusOne), None)
			, (Int.MinValue, Option(BigDecimal(Int.MinValue)), Option(BigInt(Int.MinValue)), Option(Int.MinValue), Option(Int.MinValue), Option(Int.MinValue), Option(Int.MinValue))
			, (intMinMinusOne, Option(BigDecimal(intMinMinusOne)), Option(BigInt(intMinMinusOne)), Option(intMinMinusOne), None, Option(intMinMinusOne), None)
			, (Long.MaxValue, Option(BigDecimal(Long.MaxValue)), Option(BigInt(Long.MaxValue)), Option(Long.MaxValue), Option(Long.MaxValue), Option(Long.MaxValue), None)
		)
		
		tests.foreach{x =>
			describe("In Value: " + x._1) {
				it ("convert to BigDec: " + x._2) {assertResult(x._2){LongNumeric.tryToBigDecimal(x._1)}}
				it ("convert to BigInt: " + x._3) {assertResult(x._3){LongNumeric.tryToBigInt(x._1)}}
				it ("convert to Double: " + x._4) {assertResult(x._4){LongNumeric.tryToDouble(x._1)}}
				it ("convert to Float: " + x._5) {assertResult(x._5){LongNumeric.tryToFloat(x._1)}}
				it ("convert to Long: " + x._6) {assertResult(x._6){LongNumeric.tryToLong(x._1)}}
				it ("convert to Int: " + x._7) {assertResult(x._7){LongNumeric.tryToInt(x._1)}}
			}
		}
	}
	describe("DoubleNumeric") {
		val tests = Seq[(Double, Option[BigDecimal], Option[BigInt],
				Option[Double], Option[Float], Option[Long], Option[Int])](
			  (0, Option(BigDecimal(0)), Option(BigInt(0)), Option(0), Option(0), Option(0), Option(0))
			, (1, Option(BigDecimal(1)), Option(BigInt(1)), Option(1), Option(1), Option(1), Option(1))
			, (-1, Option(BigDecimal(-1)), Option(BigInt(-1)), Option(-1), Option(-1), Option(-1), Option(-1))
			, (Int.MaxValue, Option(BigDecimal(Int.MaxValue)), Option(BigInt(Int.MaxValue)), Option(Int.MaxValue), None, Option(Int.MaxValue), Option(Int.MaxValue))
			, (intMaxPlusOne, Option(BigDecimal(intMaxPlusOne)), Option(BigInt(intMaxPlusOne)), Option(intMaxPlusOne), Option(intMaxPlusOne), Option(intMaxPlusOne), None)
			, (Int.MinValue, Option(BigDecimal(Int.MinValue)), Option(BigInt(Int.MinValue)), Option(Int.MinValue), Option(Int.MinValue), Option(Int.MinValue), Option(Int.MinValue))
			, (intMinMinusOne, Option(BigDecimal(intMinMinusOne)), Option(BigInt(intMinMinusOne)), Option(intMinMinusOne), None, Option(intMinMinusOne), None)
			, (1.5, Option(BigDecimal(1.5)), None, Option(1.5), Option(1.5F), None, None)
			, (1e20, Option(BigDecimal(1e20)), Option(BigInt(10).pow(20)), Option(1e20), None, None, None)
			, (math.pow(2, 80), Option(BigDecimal(2, UNLIMITED).pow(80)), Option(BigInt(2).pow(80)), Option(math.pow(2, 80)), Option(math.pow(2, 80).floatValue), None, None)
		)
		
		tests.foreach{x =>
			describe("In Value: " + x._1) {
				it ("convert to BigDec: " + x._2) {assertResult(x._2){DoubleNumeric.tryToBigDecimal(x._1)}}
				it ("convert to BigInt: " + x._3) {assertResult(x._3){DoubleNumeric.tryToBigInt(x._1)}}
				it ("convert to Double: " + x._4) {assertResult(x._4){DoubleNumeric.tryToDouble(x._1)}}
				it ("convert to Float: " + x._5) {assertResult(x._5){DoubleNumeric.tryToFloat(x._1)}}
				it ("convert to Long: " + x._6) {assertResult(x._6){DoubleNumeric.tryToLong(x._1)}}
				it ("convert to Int: " + x._7) {assertResult(x._7){DoubleNumeric.tryToInt(x._1)}}
			}
		}
	}
	describe("FloatNumeric") {
		val tests = Seq[(Float, Option[BigDecimal], Option[BigInt],
				Option[Double], Option[Float], Option[Long], Option[Int])](
			  (0, Option(BigDecimal(0)), Option(BigInt(0)), Option(0), Option(0), Option(0), Option(0))
			, (1, Option(BigDecimal(1)), Option(BigInt(1)), Option(1), Option(1), Option(1), Option(1))
			, (-1, Option(BigDecimal(-1)), Option(BigInt(-1)), Option(-1), Option(-1), Option(-1), Option(-1))
			, (1.5f, Option(BigDecimal("1.5")), None, Option(1.5), Option(1.5F), None, None)
			, (math.pow(2, 80).floatValue, Option(BigDecimal(2, UNLIMITED).pow(80)), Option(BigInt(2).pow(80)), Option(math.pow(2, 80)), Option(math.pow(2, 80).floatValue), None, None)
		)
		
		tests.foreach{x =>
			describe("In Value: " + x._1) {
				it ("convert to BigDec: " + x._2) {assertResult(x._2){FloatNumeric.tryToBigDecimal(x._1)}}
				it ("convert to BigInt: " + x._3) {assertResult(x._3){FloatNumeric.tryToBigInt(x._1)}}
				it ("convert to Double: " + x._4) {assertResult(x._4){FloatNumeric.tryToDouble(x._1)}}
				it ("convert to Float: " + x._5) {assertResult(x._5){FloatNumeric.tryToFloat(x._1)}}
				it ("convert to Long: " + x._6) {assertResult(x._6){FloatNumeric.tryToLong(x._1)}}
				it ("convert to Int: " + x._7) {assertResult(x._7){FloatNumeric.tryToInt(x._1)}}
			}
		}
	}
	describe("HalfFloatNumeric") {
		val tests = Seq[(Short, Option[BigDecimal], Option[BigInt],
				Option[Double], Option[Float], Option[Long], Option[Int])](
			  (0x0000.shortValue, Option(BigDecimal(0)), Option(BigInt(0)), Option(0), Option(0), Option(0), Option(0))
			, (0x8000.shortValue, Option(BigDecimal(-0.0)), Option(BigInt(0)), Option(-0.0), Option(-0.0f), Option(0), Option(0))
			, (0x3C00.shortValue, Option(BigDecimal(1)), Option(BigInt(1)), Option(1), Option(1), Option(1), Option(1))
			, (0x4000.shortValue, Option(BigDecimal(2)), Option(BigInt(2)), Option(2), Option(2), Option(2), Option(2))
			, (0x3800.shortValue, Option(BigDecimal("0.5")), None, Option(0.5), Option(0.5f), None, None)
			, (0xBC00.shortValue, Option(BigDecimal(-1)), Option(BigInt(-1)), Option(-1), Option(-1), Option(-1), Option(-1))
			, (0x3E00.shortValue, Option(BigDecimal("1.5")), None, Option(1.5), Option(1.5F), None, None)
			, (0x3555.shortValue, Option(BigDecimal("0.333251953125")), None, Option(0.333251953125), Option(0.333251953125f), None, None)
			, (0x7BFF.shortValue, Option(BigDecimal(65504)), Option(BigInt(65504)), Option(65504), Option(65504), Option(65504), Option(65504))
			, (0xC400.shortValue, Option(BigDecimal(-4)), Option(BigInt(-4)), Option(-4), Option(-4), Option(-4), Option(-4))
		)
		
		tests.foreach{x =>
			describe("In Value: 0x" + x._1.intValue.toHexString) {
				it ("convert to BigDec: " + x._2) {assertResult(x._2){HalfFloatNumeric.tryToBigDecimal(x._1)}}
				it ("convert to BigInt: " + x._3) {assertResult(x._3){HalfFloatNumeric.tryToBigInt(x._1)}}
				it ("convert to Double: " + x._4) {assertResult(x._4){HalfFloatNumeric.tryToDouble(x._1)}}
				it ("convert to Float: " + x._5) {assertResult(x._5){HalfFloatNumeric.tryToFloat(x._1)}}
				it ("convert to Long: " + x._6) {assertResult(x._6){HalfFloatNumeric.tryToLong(x._1)}}
				it ("convert to Int: " + x._7) {assertResult(x._7){HalfFloatNumeric.tryToInt(x._1)}}
			}
		}
	}
	describe("RationalNumeric") {
		val tests = Seq[(Rational, Option[BigDecimal], Option[BigInt],
				Option[Double], Option[Float], Option[Long], Option[Int])](
			  (new Rational(0,1), Option(BigDecimal("0")), Option(BigInt(0)), Option(0), Option(0), Option(0), Option(0))
			, (new Rational(0,15), Option(BigDecimal("0")), Option(BigInt(0)), Option(0), Option(0), Option(0), Option(0))
			, (new Rational(1,1), Option(BigDecimal("1")), Option(BigInt(1)), Option(1), Option(1), Option(1), Option(1))
			, (new Rational(5,5), Option(BigDecimal("1")), Option(BigInt(1)), Option(1), Option(1), Option(1), Option(1))
			, (new Rational(2,1), Option(BigDecimal("2")), Option(BigInt(2)), Option(2), Option(2), Option(2), Option(2))
			, (new Rational(1,2), Option(BigDecimal("0.5")), None, Option(0.5), Option(0.5f), None, None)
			, (new Rational(-1,1), Option(BigDecimal("-1")), Option(BigInt(-1)), Option(-1), Option(-1), Option(-1), Option(-1))
			, (new Rational(1,-1), Option(BigDecimal("-1")), Option(BigInt(-1)), Option(-1), Option(-1), Option(-1), Option(-1))
			, (new Rational(3,2), Option(BigDecimal("1.5")), None, Option(1.5), Option(1.5F), None, None)
			, (new Rational(1,3), None, None, None, None, None, None)
		)
		
		tests.foreach{x =>
			describe("In Value: " + x._1.toString) {
				it ("convert to BigDec: " + x._2) {assertResult(x._2){RationalNumeric.tryToBigDecimal(x._1)}}
				it ("convert to BigInt: " + x._3) {assertResult(x._3){RationalNumeric.tryToBigInt(x._1)}}
				it ("convert to Double: " + x._4) {assertResult(x._4){RationalNumeric.tryToDouble(x._1)}}
				it ("convert to Float: " + x._5) {assertResult(x._5){RationalNumeric.tryToFloat(x._1)}}
				it ("convert to Long: " + x._6) {assertResult(x._6){RationalNumeric.tryToLong(x._1)}}
				it ("convert to Int: " + x._7) {assertResult(x._7){RationalNumeric.tryToInt(x._1)}}
			}
		}
	}
}	
