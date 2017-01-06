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

import java.math.MathContext.UNLIMITED
import scala.math.{BigInt, BigDecimal}

/**
 * A typeclass representing a set of functions that can convert to various numeric formats
 * and will fail if that conversion would loose precision
 * @since next
 */
trait Numeric[A]{
	
	/**
	 * Call one of the second-parameter-list's parameters using a value equivalent to `a` and return the result.
	 * It is not defined which function is called if multiple are applicable.
	 *
	 * This will probably be one of the least stable methods in this library,
	 * extended if something else is deemed numeric which cannot fit inside either
	 * a BigDecimal or a Rational. But, at the same time, this function will
	 * remain a complete function, whereas every other function will have some
	 * cases which will return Option.
	 * 
	 * I imagine most common deserialization cases are happy failing if they
	 * recieve unexpected data, in which case the `tryTo` methods will be more
	 * than sufficient, but if one needs a complete function *and to be told via
	 * compiler error when said function becomes imcomplete* (which would exclude
	 * chained `Option::flatMap`s), then one would need that function.
	 */
	def fold[B](a:A)(fbd:BigDecimal => B, fr:Numeric.Rational => B):B
	
	def tryToSpecialValue(a:A):Option[Numeric.SpecialValue]
	
	/** Return a Rational with a value equal to this, if there is such a Rational */
	def tryToRational(a:A):Option[Numeric.Rational]
	/** Return a BigDecimal with a value equal to this, if there is such a BigDecmial */
	def tryToBigDecimal(a:A):Option[BigDecimal]
	/** Return a BigInt with a value equal to this, if there is such a BigInt */
	def tryToBigInt(a:A):Option[BigInt]
	/** Return a Double with a value equal to this, if there is such a Double */
	def tryToDouble(a:A):Option[Double]
	/** Return a Float with a value equal to this, if there is such a Float */
	def tryToFloat(a:A):Option[Float] = this.tryToDouble(a).collect{
		case x if (x.isNaN) => Float.NaN
		case x if (x == x.floatValue.doubleValue) => x.floatValue
	}
	/** Return a Long with a value equal to this, if there is such a Long */
	def tryToLong(a:A):Option[Long] = this.tryToBigInt(a).collect{case x if (Long.MinValue <= x && x <= Long.MaxValue) => x.longValue}
	/** Return an Int with a value equal to this, if there is such an Int */
	def tryToInt(a:A):Option[Int] = this.tryToBigInt(a).collect{case x if (Int.MinValue <= x && x <= Int.MaxValue) => x.intValue}
}

/**
 * @since next
 */
object Numeric {
	
	// Going through all the effort to deconstruct then reconstruct the floats
	// as neither BigDecimal.apply nor BigDecimal.valueOf gave the result I wanted
	// (an as-precise-as-possible translation)
	/** Build a BigDecimal from the parts of an IEEE floating point number. Assumes not NaN nor Infinity */
	private[this] def buildBigDecimal(floatParts:(Byte, Short, Long), zeroExponent:Short, significandBitCount:Byte):BigDecimal = {
		val (sign, exponent, significand) = floatParts
		if (exponent == zeroExponent) {
			if (significand == (1L << significandBitCount)) {
				BigDecimal(0)
			} else {
				throw new UnsupportedOperationException("Subnormals")
			}
		} else {(
			BigDecimal(significand, UNLIMITED)
				* (if (sign < 0) {-1} else {1})	
				* BigDecimal(2).pow(exponent)
				/ BigDecimal(2).pow(significandBitCount)
		)}
	}
	
	/** Build a BigInt from the parts of an IEEE floating point number. Assumes is whole and not NaN nor Infinity */
	private[this] def buildBigInt(floatParts:(Byte, Short, Long), zeroExponent:Short, significandBits:Byte):BigInt = {
		val (sign, exponent, significand) = floatParts
		if (exponent == zeroExponent) {
			if (significand == (1L << significandBits)) {
				BigInt(0)
			} else {
				throw new UnsupportedOperationException("Subnormals")
			}
		} else {(
			BigInt(significand)
				* (if (sign < 0) {-1} else {1})	
				* BigInt(2).pow(exponent)
				/ BigInt(2).pow(significandBits)
		)}
	}
	
	/** Build a Rational from the parts of an IEEE floating point number. Assumes not NaN nor Infinity */
	private[this] def buildRational(floatParts:(Byte, Short, Long), zeroExponent:Short, significandBitCount:Byte):Rational = {
		val (sign, exponent, significand) = floatParts
		if (exponent == zeroExponent) {
			if (significand == (1L << significandBitCount)) {
				new Rational(0, 1)
			} else {
				throw new UnsupportedOperationException("Subnormals")
			}
		} else {(
			if (exponent > 0) {
				new Rational(
					BigInt(significand)
						* (if (sign < 0) {-1} else {1})
						* BigInt(2).pow(exponent),
					BigInt(2).pow(significandBitCount)
				)
			} else {
				new Rational(
					BigInt(significand)
						* (if (sign < 0) {-1} else {1}),
					BigInt(2).pow(significandBitCount - exponent)
				)
			}
		)}
	}
	
	
	/** For when something need not know what `A` is, just that there is a numeric for it */
	final case class NumericPair[A](a:A)(implicit Num:Numeric[A]) {
		def fold[B](fbd:BigDecimal => B, fr:Numeric.Rational => B):B = Num.fold(a)(fbd, fr)
		def tryToRational:Option[Numeric.Rational] = a.tryToRational
		def tryToSpecialValue:Option[Numeric.SpecialValue] = a.tryToSpecialValue
		def tryToBigDecimal:Option[BigDecimal] = a.tryToBigDecimal
		def tryToBigInt:Option[BigInt] = a.tryToBigInt
		def tryToDouble:Option[Double] = a.tryToDouble
		def tryToFloat:Option[Float] = a.tryToFloat
		def tryToLong:Option[Long] = a.tryToLong
		def tryToInt:Option[Int] = a.tryToInt
	}
	
	implicit object BigDecimalNumeric extends Numeric[BigDecimal] {
		override def fold[B](a:BigDecimal)(fbd:BigDecimal => B, fr:Numeric.Rational => B):B = fbd(a)
		override def tryToSpecialValue(a:BigDecimal):Option[SpecialValue] = None
		
		override def tryToRational(a:BigDecimal):Option[Rational] = {
			val ulp = a.ulp
			if (ulp > 1) {
				Option(new Rational(a.toBigInt, 1))
			} else {
				Option(new Rational((a / ulp).toBigInt, (1 / ulp).toBigInt))
			}
		}
		override def tryToBigDecimal(a:BigDecimal):Option[BigDecimal] = Some(a:BigDecimal)
		override def tryToBigInt(a:BigDecimal):Option[BigInt] = if (a.isWhole) { a.toBigIntExact } else {None}
		override def tryToDouble(a:BigDecimal):Option[Double] = if ( BigDecimal(a.doubleValue) == a) {Option(a.doubleValue)} else {None}
		override def tryToLong(a:BigDecimal):Option[Long] = if (BigDecimal(a.longValue) == a) {Option(a.longValue)} else {None}
	}
	
	implicit object BigIntNumeric extends Numeric[BigInt] {
		override def fold[B](a:BigInt)(fbd:BigDecimal => B, fr:Numeric.Rational => B):B = fbd(BigDecimal(a))
		override def tryToSpecialValue(a:BigInt):Option[SpecialValue] = None
		
		override def tryToRational(a:BigInt):Option[Rational] = Option(new Rational(a, 1))
		override def tryToBigDecimal(a:BigInt):Option[BigDecimal] = Option(BigDecimal(a))
		override def tryToBigInt(a:BigInt):Option[BigInt] = Option(a)
		override def tryToDouble(a:BigInt):Option[Double] = DoubleNumeric.tryToBigInt(a.doubleValue).collect{case x if x == a => a.doubleValue}
	}
	
	implicit object DoubleNumeric extends Numeric[Double] {
		private[this] def split(a:Double):(Byte, Short, Long) = {
			val bits = java.lang.Double.doubleToLongBits(a)
			val signBitRaw = bits & 0x8000000000000000L
			val exponentBitsRaw = bits & 0x7FF0000000000000L
			val significandBitsRaw = bits & 0x000FFFFFFFFFFFFFL
			val sign:Byte = if (signBitRaw == 0) {0} else {-1}
			val exponentBits:Short =  (exponentBitsRaw >> significandBitCount).shortValue
			val exponent = (exponentBits - 1023).shortValue
			val significandBits = significandBitsRaw | 0x0010000000000000L
			(sign, exponent, significandBits)
		}
		val zeroExponent:Short = -1023
		val significandBitCount:Byte = 52
		
		override def fold[B](a:Double)(fbd:BigDecimal => B, fr:Numeric.Rational => B):B = fr(this.toRational(a))
		
		override def tryToRational(a:Double):Option[Rational] = Option(this.toRational(a))
		private[this] def toRational(a:Double):Rational = {
			if (a.isNaN) {new Rational(0,0)}
			else if (a.isPosInfinity) {new Rational(1,0)}
			else if (a.isNegInfinity) {new Rational(-1,0)}
			else {buildRational(this.split(a), zeroExponent, significandBitCount)}
		}
		
		override def tryToSpecialValue(a:Double):Option[SpecialValue] = {
			if (a.isNaN) {Option(Numeric.NaN)}
			else if (a.isPosInfinity) {Option(Numeric.PositiveInfinity)}
			else if (a.isNegInfinity) {Option(Numeric.NegativeInfinity)}
			else {None}
		}
		
		override def tryToBigDecimal(a:Double):Option[BigDecimal] = {
			this.tryToSpecialValue(a).fold(Option(buildBigDecimal(this.split(a), zeroExponent, significandBitCount))){x => None}
		}
		override def tryToBigInt(a:Double):Option[BigInt] = if (a.isWhole) {Option(buildBigInt(this.split(a), zeroExponent, significandBitCount))} else {None}
		override def tryToDouble(a:Double):Option[Double] = Option(a)
		override def tryToLong(a:Double):Option[Long] = if (a.longValue.doubleValue == a) { Option(a.longValue) } else {None}
		override def tryToInt(a:Double):Option[Int] = if (a.isValidInt) { Option(a.intValue) } else {None}
	}
	
	implicit object FloatNumeric extends Numeric[Float] {
		private[this] def split(a:Float):(Byte, Short, Long) = {
			val bits = java.lang.Float.floatToIntBits(a)
			val signBitRaw = bits & 0x80000000
			val exponentBitsRaw = bits & 0x7F800000
			val significandBitsRaw = bits & 0x007FFFFF
			val sign:Byte = if (signBitRaw == 0) {0} else {-1}
			val exponentBits:Short =  (exponentBitsRaw >> 23).shortValue
			val exponent = (exponentBits - 127).shortValue
			val significandBits = significandBitsRaw | 0x00800000
			(sign, exponent, significandBits)
		}
		private[this] val zeroExponent:Short = -127
		private[this] val significandBitCount:Byte = 23
		
		override def fold[B](a:Float)(fbd:BigDecimal => B, fr:Numeric.Rational => B):B = fr(this.toRational(a))
		
		override def tryToRational(a:Float):Option[Rational] = Option(this.toRational(a))
		private[this] def toRational(a:Float):Rational = {
			if (a.isNaN) {new Rational(0,0)}
			else if (a.isPosInfinity) {new Rational(1,0)}
			else if (a.isNegInfinity) {new Rational(-1,0)}
			else {buildRational(this.split(a), zeroExponent, significandBitCount)}
		}
		
		override def tryToSpecialValue(a:Float):Option[SpecialValue] = {
			if (a.isNaN) {Option(Numeric.NaN)}
			else if (a.isPosInfinity) {Option(Numeric.PositiveInfinity)}
			else if (a.isNegInfinity) {Option(Numeric.NegativeInfinity)}
			else {None}
		}
		
		override def tryToBigDecimal(a:Float):Option[BigDecimal] = {
			this.tryToSpecialValue(a).fold(Option(buildBigDecimal(this.split(a), zeroExponent, significandBitCount))){x => None}
		}
		override def tryToBigInt(a:Float):Option[BigInt] = if (a.isWhole) {Option(buildBigInt(this.split(a), zeroExponent, significandBitCount))} else {None}
		override def tryToDouble(a:Float):Option[Double] = Option(a)
		override def tryToFloat(a:Float):Option[Float] = Option(a)
		override def tryToLong(a:Float):Option[Long] = if (a.longValue.floatValue == a) { Option(a.longValue) } else {None}
	}
	
	/** A Numeric that treats a Short as if it contains the bit-sequence for encoding a half-width floating point */
	object HalfFloatNumeric extends Numeric[Short] {
		private[this] def split(bits:Short):(Byte, Short, Long) = {
			val signBitRaw = bits & 0x8000
			val exponentBitsRaw = bits & 0x7C00
			val significandBitsRaw = bits & 0x03FF
			val sign:Byte = if (signBitRaw == 0) {0} else {-1}
			val exponentBits:Short =  (exponentBitsRaw >> significandBitCount).shortValue
			val exponent = (exponentBits - 15).shortValue
			val significandBits = significandBitsRaw | 0x0400
			(sign, exponent, significandBits)
		}
		private[this] val zeroExponent:Short = -15
		private[this] val significandBitCount:Byte = 10
		
		override def fold[B](a:Short)(fbd:BigDecimal => B, fr:Numeric.Rational => B):B = fr(this.toRational(a))
		
		override def tryToRational(a:Short):Option[Rational] = Option(this.toRational(a))
		private[this] def toRational(a:Short):Rational = {
			if (a == 0x7C00) {new Rational(1,0)}
			else if (a == 0xFC00.shortValue) {new Rational(-1,0)}
			else if ((a & 0x7C00) == 0x7C00) {new Rational(0,0)}
			else {buildRational(this.split(a), zeroExponent, significandBitCount)}
		}
		
		override def tryToSpecialValue(a:Short):Option[SpecialValue] = {
			if (a == 0x7C00) {Option(Numeric.PositiveInfinity)}
			else if (a == 0xFC00.shortValue) {Option(Numeric.NegativeInfinity)}
			else if ((a & 0x7C00) == 0x7C00) {Option(Numeric.NaN)}
			else {None}
		}
		
		override def tryToBigDecimal(a:Short):Option[BigDecimal] = {
			this.tryToSpecialValue(a).fold(Option(buildBigDecimal(this.split(a), zeroExponent, significandBitCount))){x => None}
		}
		override def tryToBigInt(a:Short):Option[BigInt] = this.tryToBigDecimal(a).collect{case x if (x.isWhole) => x.toBigIntExact}.flatten
		override def tryToDouble(a:Short):Option[Double] = Option{
			if (a == 0) {0.0}
			else if (a == 0x8000.shortValue) {-0.0}
			else {
				this.tryToSpecialValue(a).map{_ match {
					case Numeric.PositiveInfinity => Double.PositiveInfinity
					case Numeric.NegativeInfinity => Double.NegativeInfinity
					case Numeric.NaN => Double.NaN
				}}.getOrElse{
					val (sign, expon, signif) = split(a)
					val sign2 = if (sign < 0) {0x8000000000000000L} else {0}
					val expon2 = (expon.longValue - DoubleNumeric.zeroExponent) << 52
					val signif2 = (signif & 0x03FFL) << (52 - 10)
					java.lang.Double.longBitsToDouble(sign2 | expon2 | signif2)
				}
			}
		}
		override def tryToLong(a:Short):Option[Long] = {
			this.tryToDouble(a).collect{case x if x.isWhole => x.longValue}
		}
	}
	
	implicit object IntNumeric extends Numeric[Int] {
		override def fold[B](a:Int)(fbd:BigDecimal => B, fr:Numeric.Rational => B):B = fbd(a:BigDecimal)
		
		override def tryToSpecialValue(a:Int):Option[SpecialValue] = None
		override def tryToRational(a:Int):Option[Rational] = Option(new Rational(a, 1))
		override def tryToBigDecimal(a:Int):Option[BigDecimal] = Some(a:BigDecimal)
		override def tryToBigInt(a:Int):Option[BigInt] = Option(a:BigInt)
		override def tryToDouble(a:Int):Option[Double] = Option(a:Double)
		override def tryToLong(a:Int):Option[Long] = Option(a)
		override def tryToInt(a:Int):Option[Int] = Option(a)
	}
	
	implicit object LongNumeric extends Numeric[Long] {
		override def fold[B](a:Long)(fbd:BigDecimal => B, fr:Numeric.Rational => B):B = fbd(a:BigDecimal)
		
		override def tryToSpecialValue(a:Long):Option[SpecialValue] = None
		override def tryToRational(a:Long):Option[Rational] = Option(new Rational(a, 1))
		override def tryToBigDecimal(a:Long):Option[BigDecimal] = Some(a:BigDecimal)
		override def tryToBigInt(a:Long):Option[BigInt] = Option(a:BigInt)
		override def tryToDouble(a:Long):Option[Double] = {
			if (a.doubleValue.longValue == a &&
					Long.MinValue <= BigDecimal(a.doubleValue) &&
					BigDecimal(a.doubleValue) <= Long.MaxValue) {
				Option(a:Double)
			} else {
				None
			}
		}
		override def tryToLong(a:Long):Option[Long] = Option(a)
		override def tryToInt(a:Long):Option[Int] = if (a.isValidInt) { Option(a.intValue) } else {None}
	}
	
	/** A value represeting a whole number divided by another whole number */
	final class Rational(num:BigInt, denom:BigInt) {
		def isNaN:Boolean = denom == 0 && num == 0
		def isPosInfinity:Boolean = denom == 0 && num > 0
		def isNegInfinity:Boolean = denom == 0 && num < 0
		def isWhole:Boolean = denom != 0 && (num mod denom.abs) == BigInt(0)
		def toBigDecimal:BigDecimal = BigDecimal(num, java.math.MathContext.UNLIMITED) / BigDecimal(denom, java.math.MathContext.UNLIMITED)
		def tryToBigDecimal:Option[BigDecimal] = (try {Option(this.toBigDecimal)} catch {case e:java.lang.ArithmeticException => None})
		def toBigInt:BigInt = num / denom
		override def toString:String = this.num.toString + "/" + this.denom.toString
		override def equals(other2:Any):Boolean = other2 match {
			case other:Rational => {
				// NaN doesn't equal NaN for either Float type; so also not doing so here
				if (this.isNaN) { false }
				else if (other.isNaN) { false }
				else if (this.isPosInfinity) { other.isPosInfinity }
				else if (this.isNegInfinity) { other.isNegInfinity }
				else { this.tryToBigDecimal == other.tryToBigDecimal }
			}
			case _ => false
		}
	}
	
	implicit object RationalNumeric extends Numeric[Rational] {
		override def fold[B](a:Rational)(fbd:BigDecimal => B, fr:Numeric.Rational => B):B = fr(a)
		
		override def tryToSpecialValue(a:Rational):Option[SpecialValue] = {
			if (a.isNaN) {Option(Numeric.NaN)}
			else if (a.isPosInfinity) {Option(Numeric.PositiveInfinity)}
			else if (a.isNegInfinity) {Option(Numeric.NegativeInfinity)}
			else {None}
		}
		
		override def tryToRational(a:Rational):Option[Rational] = Option(a)
		override def tryToBigDecimal(a:Rational):Option[BigDecimal] = a.tryToBigDecimal
		override def tryToBigInt(a:Rational):Option[BigInt] = if(a.isWhole) {Option(a.toBigInt)} else {None}
		override def tryToDouble(a:Rational):Option[Double] = {
			this.tryToSpecialValue(a).map{_ match {
				case Numeric.PositiveInfinity => Double.PositiveInfinity
				case Numeric.NegativeInfinity => Double.NegativeInfinity
				case Numeric.NaN => Double.NaN
			}}.orElse{
				this.tryToBigDecimal(a).collect{case a if (BigDecimal(a.doubleValue) == a) => a.doubleValue}
			}
		}
		override def tryToLong(a:Rational):Option[Long] = this.tryToBigInt(a).collect{case a if (Long.MinValue <= a && a <= Long.MaxValue) => a.longValue}
	}
	
	
	implicit class NumericSyntax[A](self:A)(implicit Num:Numeric[A]) {
		def tryToSpecialValue:Option[SpecialValue] = Num.tryToSpecialValue(self)
		def tryToRational:Option[Rational] = Num.tryToRational(self)
		def tryToBigDecimal:Option[BigDecimal] = Num.tryToBigDecimal(self)
		def tryToBigInt:Option[BigInt] = Num.tryToBigInt(self)
		def tryToDouble:Option[Double] = Num.tryToDouble(self)
		def tryToFloat:Option[Float] = Num.tryToFloat(self)
		def tryToLong:Option[Long] = Num.tryToLong(self)
		def tryToInt:Option[Int] = Num.tryToInt(self)
	}
	
	/** Represents the not-real values a double may take */
	sealed trait SpecialValue
	final object NaN extends SpecialValue
	final object PositiveInfinity extends SpecialValue
	final object NegativeInfinity extends SpecialValue
}
