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
import com.rayrobdod.json.union.CborValue.Rational
import java.math.MathContext.UNLIMITED

class CborValueRationalTest extends FunSpec {
	
	private val intMaxPlusOne = (Int.MaxValue:Long) + 1
	private val intMinMinusOne = (Int.MinValue:Long) - 1
	private val longMaxPlusOne = (Long.MaxValue:BigInt) + 1
	private val longMinMinusOne = (Long.MinValue:BigInt) - 1
	
	describe("Rational ops") {
		it ("NaN isNaN") { assert(Rational.NaN.isNaN) }
		it ("PosInf isPosInf") { assert(Rational.PositiveInfinity.isPosInfinity) }
		it ("NegInf isNegInf") { assert(Rational.NegativeInfinity.isNegInfinity) }
		
		describe("equals") {
			it ("1/2 == 2/4") {
				assertResult(new Rational(1,2)){new Rational(2,4)}
			}
			it ("1/2 != Bob") {
				assert(new Rational(1,2) != "Bob")
			}
			it ("1/52 != 52/1") {
				assert(new Rational(1,52) != new Rational(52,1))
			}
			it ("NaN != NaN") {
				assert(Rational.NaN != Rational.NaN)
			}
			it ("PosInf == PosInf") {
				for (
					a <- 1 to 30;
					b <- 1 to 30
				) {
					assertResult(new Rational(a, 0)){new Rational(b, 0)}
				}
			}
			it ("NegInf == NegInf") {
				for (
					a <- 1 to 30;
					b <- 1 to 30
				) {
					assertResult(new Rational(-a, 0)){new Rational(-b, 0)}
				}
			}
			it ("NegInf != Real") {
				for (
					a <- 1 to 30;
					b <- -2.0 to 2.0 by 0.25
				) {
					assert(new Rational(-a, 0) != Rational(b))
				}
			}
			it ("PosInf != Real") {
				for (
					a <- 1 to 30;
					b <- -2.0 to 2.0 by 0.25
				) {
					assert(new Rational(a, 0) != Rational(b))
				}
			}
			it ("Real != NegInf") {
				for (
					a <- 1 to 30;
					b <- -2.0 to 2.0 by 0.25
				) {
					assert(Rational(b) != new Rational(-a, 0))
				}
			}
			it ("Real != PosInf") {
				for (
					a <- 1 to 30;
					b <- -2.0 to 2.0 by 0.25
				) {
					assert(Rational(b) != new Rational(a, 0))
				}
			}
		}
		describe("hashCode") {
			describe ("is equal when rationals are equal") {
				val nums = 0 :: 1 :: 2 :: 3 :: 4 :: -1 :: -2 :: 20 :: 10 :: -10 :: -20 :: 52 :: Nil
				for (a <- nums; b <- nums; c <- nums; d <- nums) {
					val ab = new Rational(a,b)
					val cd = new Rational(c,d)
					if (ab == cd) {
						it (s"$ab and $cd") {
							assert(ab.hashCode == cd.hashCode)
						}
					}
				}
			}
		}
		
		describe ("toDouble") {
			it ("PosInf") {
				assertResult(Double.PositiveInfinity){Rational.PositiveInfinity.toDouble}
			}
			it ("NegInf") {
				assertResult(Double.NegativeInfinity){Rational.NegativeInfinity.toDouble}
			}
			it ("NaN") {
				assert(Rational.NaN.toDouble.isNaN)
			}
			it ("toDouble provides a value even when no precise value is avaliable") {
				assert(math.abs(0.33333 - new Rational(1,3).toDouble) <= 0.0001)
			}
		}
		describe("reduce") {
			it ("returns an equivalent value with the smallest possible num and denom") {
				assertResult("6/12"){new Rational(6, 12).toString}
				assertResult("1/2"){new Rational(6, 12).reduce.toString}
			}
			it ("the denominator is always nonnegative after reduction") {
				assertResult("2/-3"){new Rational(2, -3).toString}
				assertResult("-2/3"){new Rational(2, -3).reduce.toString}
			}
			it ("posinf") {
				assertResult(1){new Rational(5, 0).reduce.num}
				assertResult(0){new Rational(5, 0).reduce.denom}
			}
			it ("neginf") {
				assertResult(-1){new Rational(-5, 0).reduce.num}
				assertResult(0){new Rational(-5, 0).reduce.denom}
			}
			it ("nan") {
				assertResult(0){new Rational(0, 0).reduce.num}
				assertResult(0){new Rational(0, 0).reduce.denom}
			}
		}
	}
	
	describe("Int conversions") {
		val happy = Seq[Int](
			  0, 1, -1
			, Int.MaxValue, Int.MinValue
		)
		val unhappy = Seq[Rational](
			  Rational(intMaxPlusOne)
			, Rational(intMinMinusOne)
			, Rational(1.5)
			, Rational.NaN
			, Rational.PositiveInfinity
			, Rational.NegativeInfinity
		)
		
		happy.foreach{int =>
			val rat = new Rational(int, 1)
			describe(s"For value $int") {
				it ("To Rat") { assertResult(rat){Rational(int)} }
				it ("From Rat") { assertResult(Some(int)){rat.tryToInt} }
			}
		}
		unhappy.foreach{rat =>
			describe(s"For value $rat") {
				it ("cannot toint") { assertResult(None){rat.tryToInt} }
			}
		}
	}
	describe("Long conversions") {
		val happy = Seq[Long](
			  0, 1, -1
			, Int.MaxValue, Int.MinValue
			, intMaxPlusOne, intMinMinusOne
			, Long.MaxValue, Long.MinValue
		)
		val unhappy = Seq[Rational](
			  BigInt(10).pow(20)
			, longMaxPlusOne
			, longMinMinusOne
			, 1.5
			, Rational.NaN
			, Rational.PositiveInfinity
			, Rational.NegativeInfinity
		)
		
		happy.foreach{int =>
			val rat = new Rational(int, 1)
			describe(s"For value $int") {
				it ("To Rat") { assertResult(rat){Rational(int)} }
				it ("From Rat") { assertResult(Some(int)){rat.tryToLong} }
			}
		}
		unhappy.foreach{rat =>
			describe(s"For value $rat") {
				it ("cannot toint") { assertResult(None){rat.tryToLong} }
			}
		}
	}
	describe("BigInt conversions") {
		val happy = Seq[BigInt](
			  0, 1, -1
			, Int.MaxValue, Int.MinValue
			, intMaxPlusOne, intMinMinusOne
			, Long.MaxValue, Long.MinValue
			, BigInt(10).pow(20)
		)
		val unhappy = Seq[Rational](
			  Rational(1.5)
			, Rational.NaN
			, Rational.PositiveInfinity
			, Rational.NegativeInfinity
		)
		
		happy.foreach{int =>
			val rat = new Rational(int, 1)
			describe(s"For value $int") {
				it ("To Rat") { assertResult(rat){Rational(int)} }
				it ("From Rat") { assertResult(Some(int)){rat.tryToBigInt} }
			}
		}
		unhappy.foreach{rat =>
			describe(s"For value $rat") {
				it ("cannot toint") { assertResult(None){rat.tryToBigInt} }
			}
		}
	}
	
	describe("BigDecimal conversions") {
		val happy = Seq[(BigDecimal, Rational)](
			  (BigDecimal("0"), new Rational(0, 1))
			, (BigDecimal("1"), new Rational(1, 1))
			, (BigDecimal("2"), new Rational(2, 1))
			, (BigDecimal("-1"), new Rational(-1, 1))
			, (BigDecimal("0.5"), new Rational(1, 2))
			, (BigDecimal("1.5"), new Rational(3, 2))
			, (BigDecimal("1e20"), new Rational(BigInt(10).pow(20), 1))
			, (BigDecimal(2, UNLIMITED).pow(80), new Rational(BigInt(2).pow(80), 1))
		)
		val unhappy = Seq[Rational](
			  Rational.NaN
			, Rational.PositiveInfinity
			, Rational.NegativeInfinity
			, new Rational(1,3)
		)
		
		happy.foreach{(ab) =>
			val (int, rat) = ab
			describe(s"For value $int") {
				it ("To Rat") { assertResult(rat){Rational(int)} }
				it ("From Rat") { assertResult(Some(int)){rat.tryToBigDecimal} }
			}
		}
		unhappy.foreach{rat =>
			describe(s"For value $rat") {
				it ("cannot toint") { assertResult(None){rat.tryToBigDecimal} }
			}
		}
	}
	describe("Double conversions") {
		val happy = Seq[(Double, Rational)](
			  (0, new Rational(0, 1))
			, (1, new Rational(1, 1))
			, (2, new Rational(2, 1))
			, (-1, new Rational(-1, 1))
			, (0.5, new Rational(1, 2))
			, (1.5, new Rational(3, 2))
			, (1e20, new Rational(BigInt(10).pow(20), 1))
			, (4.9E-324, new Rational(1, BigInt(2).pow(1074)))
			, (-4.9E-324, new Rational(-1, BigInt(2).pow(1074)))
			, (math.pow(2, 80), new Rational(BigInt(2).pow(80), 1))
			, (-4.1D, new Rational(-18464758472219032L, 4503599627370496L))
			, (Double.PositiveInfinity, Rational.PositiveInfinity)
			, (Double.NegativeInfinity, Rational.NegativeInfinity)
		)
		val unhappy = Seq[Rational](
			  new Rational(BigInt(2).pow(80) + 1, 1)
			, new Rational(1,3)
		)
		
		describe("For value NaN") {
			it ("To Rat") { assert{Rational(Double.NaN).isNaN} }
			it ("From Rat") { assert{Rational.NaN.tryToDouble.get.isNaN} }
		}
		happy.foreach{(ab) =>
			val (int, rat) = ab
			describe(s"For value $int") {
				it ("To Rat") { assertResult(rat){Rational(int)} }
				it ("From Rat") { assertResult(Some(int)){rat.tryToDouble} }
			}
		}
		unhappy.foreach{rat =>
			describe(s"For value $rat") {
				it ("cannot toint") { assertResult(None){rat.tryToDouble} }
			}
		}
	}
	describe("Float conversions") {
		val happy = Seq[(Float, Rational)](
			  (0f, new Rational(0, 1))
			, (1f, new Rational(1, 1))
			, (2f, new Rational(2, 1))
			, (-1f, new Rational(-1, 1))
			, (0.5f, new Rational(1, 2))
			, (1.5f, new Rational(3, 2))
			, (1.4e-45f, new Rational(1, BigInt(2).pow(149)))
			, (5.6e-45f, new Rational(4, BigInt(2).pow(149)))
			, (math.pow(2, 80).floatValue, new Rational(BigInt(2).pow(80), 1))
			, (Float.PositiveInfinity, Rational.PositiveInfinity)
			, (Float.NegativeInfinity, Rational.NegativeInfinity)
		)
		val unhappy = Seq[Rational](
			  new Rational(BigInt(2).pow(80) + 1, 1)
			, new Rational(BigInt(10).pow(20), 1)
			, new Rational(1,3)
			, new Rational(1, BigInt(2).pow(1074))
		)
		
		describe("For value NaN") {
			it ("To Rat") { assert{Rational(Float.NaN).isNaN} }
			it ("From Rat") { assert{Rational.NaN.tryToFloat.get.isNaN} }
		}
		happy.foreach{(ab) =>
			val (int, rat) = ab
			describe(s"For value $int") {
				it ("To Rat") { assertResult(rat){Rational(int)} }
				it ("From Rat") { assertResult(Some(int)){rat.tryToFloat} }
			}
		}
		unhappy.foreach{rat =>
			describe(s"For value $rat") {
				it ("cannot toint") { assertResult(None){rat.tryToFloat} }
			}
		}
	}
	describe("HalfFloat conversions") {
		val happy = Seq[(Short, Rational)](
			  (0x0000.shortValue, new Rational(0, 1))
			, (0x8000.shortValue, new Rational(-0, 1))
			, (0x3C00.shortValue, new Rational(1, 1))
			, (0x4000.shortValue, new Rational(2, 1))
			, (0x3800.shortValue, new Rational(1, 2))
			, (0xBC00.shortValue, new Rational(-1, 1))
			, (0x3E00.shortValue, new Rational(3, 2))
			, (0x3555.shortValue, new Rational(1365, 4096))
			, (0x7BFF.shortValue, new Rational(65504, 1))
			, (0xC400.shortValue, new Rational(-4, 1))
			, (0x0001.shortValue, new Rational(1, 1 << 24))
			, (0x7C00.shortValue, Rational.PositiveInfinity)
			, (0xFC00.shortValue, Rational.NegativeInfinity)
		)
		val unhappy = Seq[Rational](
			  new Rational(BigInt(2).pow(80), 1)
			, new Rational(BigInt(10).pow(20), 1)
			, new Rational(100000, 1)
			, new Rational(1, 1 << 30)
		)
		
		describe("For value NaN") {
			it ("To Rat") { assert{Rational.fromHalfFloat(0x7C20.shortValue).isNaN} }
		}
		happy.foreach{(ab) =>
			val (int, rat) = ab
			describe(s"For value 0x${int.intValue.toHexString}") {
				it ("To Rat") { assertResult(rat){Rational.fromHalfFloat(int)} }
			}
		}
	}
}	
