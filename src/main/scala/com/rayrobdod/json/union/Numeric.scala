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
import scala.language.implicitConversions
import spire.math.Rational

/**
 * A typeclass represeting a set of functions that can convert to various numeric formats
 * and will fail if that conversion would loose precision
 * @since next
 */
trait Numeric[A]{
	def tryToBigDecimal(a:A):Some[BigDecimal]
	def tryToBigInt(a:A):Option[BigInt]
	def tryToDouble(a:A):Option[Double]
	def tryToFloat(a:A):Option[Float] = this.tryToDouble(a).collect{case x if (x == x.floatValue.doubleValue) => x.floatValue}
	def tryToLong(a:A):Option[Long]
	def tryToInt(a:A):Option[Int] = this.tryToLong(a).collect{case x if (Int.MinValue < x && x < Int.MaxValue) => x.intValue}
}

/**
 * @since next
 */
object Numeric {
	// Going through all the effort to deconstruct then reconstruct the floats as neither BigDecimal.apply nor BigDecimal.valueOf gave the result I wanted (an as-presice-as-possible translation)
	
	/** Build a BigDecimal from the parts of a floating point number */
	private[this] def buildBigDecimal(floatParts:(Byte, Short, Long), zeroExponent:Short, significandBitCount:Byte):BigDecimal = {
		val (sign, exponent, significand) = floatParts
		if (exponent == zeroExponent) {
			if (significand == (1L << significandBitCount)) { BigDecimal(0) } else {
				throw new UnsupportedOperationException("Subnormals")
			}
		} else {(
			BigDecimal(significand, UNLIMITED)
				* (if (sign < 0) {-1} else {1})	
				* BigDecimal(2).pow(exponent)
				/ BigDecimal(2).pow(significandBitCount)
		)}
	}
	
	/** Build a BigInt from the parts of a floating point number. Assumes is whole */
	private[this] def buildBigInt(floatParts:(Byte, Short, Long), zeroExponent:Short, significandBits:Byte):BigInt = {
		val (sign, exponent, significand) = floatParts
		if (exponent == zeroExponent) {
			if (significand == (1L << significandBits)) { BigInt(0) } else {
				throw new UnsupportedOperationException("Nans, Infinities or Subnormals")
			}
		} else {(
			BigInt(significand)
				* (if (sign < 0) {-1} else {1})	
				* BigInt(2).pow(exponent)
				/ BigInt(2).pow(significandBits)
		)}
	}
	
	
	
	case class NumericPair[A](a:A)(implicit Num:Numeric[A]) {
		def tryToBigDecimal:Some[BigDecimal] = a.tryToBigDecimal
		def tryToBigInt:Option[BigInt] = a.tryToBigInt
		def tryToDouble:Option[Double] = a.tryToDouble
		def tryToFloat:Option[Float] = a.tryToFloat
		def tryToLong:Option[Long] = a.tryToLong
		def tryToInt:Option[Int] = a.tryToInt
	}
	
	implicit object BigDecimalNumeric extends Numeric[BigDecimal] {
		override def tryToBigDecimal(a:BigDecimal):Some[BigDecimal] = Some(a:BigDecimal)
		override def tryToBigInt(a:BigDecimal):Option[BigInt] = if (a.isWhole) { a.toBigIntExact } else {None}
		override def tryToDouble(a:BigDecimal):Option[Double] = if ( BigDecimal(a.doubleValue) == a) {Option(a.doubleValue)} else {None}
		override def tryToLong(a:BigDecimal):Option[Long] = if (BigDecimal(a.longValue) == a) {Option(a.longValue)} else {None}
	}
	
	implicit object BigIntNumeric extends Numeric[BigInt] {
		override def tryToBigDecimal(a:BigInt):Some[BigDecimal] = Some(BigDecimal(a))
		override def tryToBigInt(a:BigInt):Option[BigInt] = if (a.isWhole) { Option(a) } else {None}
		override def tryToDouble(a:BigInt):Option[Double] = if ( BigInt(a.doubleValue.toString) == a) {Option(a.doubleValue)} else {None}
		override def tryToLong(a:BigInt):Option[Long] = if (Long.MinValue <= a && a <= Long.MaxValue) {Option(a.longValue)} else {None}
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
		
		override def tryToBigDecimal(a:Double):Some[BigDecimal] = Some(buildBigDecimal(this.split(a), zeroExponent, significandBitCount))
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
		
		override def tryToBigDecimal(a:Float):Some[BigDecimal] = Some(buildBigDecimal(this.split(a), zeroExponent, significandBitCount))
		override def tryToBigInt(a:Float):Option[BigInt] = if (a.isWhole) {Option(buildBigInt(this.split(a), zeroExponent, significandBitCount))} else {None}
		override def tryToDouble(a:Float):Option[Double] = Option(a)
		override def tryToFloat(a:Float):Option[Float] = Option(a)
		override def tryToLong(a:Float):Option[Long] = if (a.longValue.floatValue == a) { Option(a.longValue) } else {None}
	}
	
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
		
		override def tryToBigDecimal(a:Short):Some[BigDecimal] = Some(buildBigDecimal(this.split(a), zeroExponent, significandBitCount))
		override def tryToBigInt(a:Short):Option[BigInt] = this.tryToBigDecimal(a).collect{case x if (x.isWhole) => x.toBigIntExact}.flatten
		override def tryToDouble(a:Short):Option[Double] = Option{
			if (a == 0) {0.0} else if (a == 0x8000.shortValue) {-0.0} else {
				val (sign, expon, signif) = split(a)
				val sign2 = if (sign < 0) {0x8000000000000000L} else {0}
				val expon2 = (expon.longValue - DoubleNumeric.zeroExponent) << 52
				val signif2 = (signif & 0x03FFL) << (52 - 10)
				java.lang.Double.longBitsToDouble(sign2 | expon2 | signif2)
			}
		}
		override def tryToLong(a:Short):Option[Long] = {
			this.tryToDouble(a).collect{case x if x.isWhole => x.longValue}
		}
	}
	
	implicit object IntNumeric extends Numeric[Int] {
		override def tryToBigDecimal(a:Int):Some[BigDecimal] = Some(a:BigDecimal)
		override def tryToBigInt(a:Int):Option[BigInt] = Option(a:BigInt)
		override def tryToDouble(a:Int):Option[Double] = Option(a:Double)
		override def tryToLong(a:Int):Option[Long] = Option(a)
		override def tryToInt(a:Int):Option[Int] = Option(a)
	}
	
	implicit object LongNumeric extends Numeric[Long] {
		override def tryToBigDecimal(a:Long):Some[BigDecimal] = Some(a:BigDecimal)
		override def tryToBigInt(a:Long):Option[BigInt] = Option(a:BigInt)
		override def tryToDouble(a:Long):Option[Double] = if (a.doubleValue.longValue == a) {Option(a:Double)} else {None}
		override def tryToLong(a:Long):Option[Long] = Option(a)
		override def tryToInt(a:Long):Option[Int] = if (a.isValidInt) { Option(a.intValue) } else {None}
	}
	
	implicit object RationalNumeric extends Numeric[Rational] {
		override def tryToBigDecimal(a:Rational):Some[BigDecimal] = Some(a.toBigDecimal(java.math.MathContext.UNLIMITED))
		override def tryToBigInt(a:Rational):Option[BigInt] = if(a.isWhole) {Option(a.toBigInt)} else {None}
		override def tryToDouble(a:Rational):Option[Double] = if (BigDecimal(a.doubleValue) == a.toBigDecimal(java.math.MathContext.UNLIMITED)) {Option(a.doubleValue)} else {None}
		override def tryToLong(a:Rational):Option[Long] = this.tryToBigInt(a).collect{case a if (Long.MinValue <= a && a <= Long.MaxValue) => a.longValue}
	}
	
	
	implicit class NumericSyntax[A](self:A)(implicit Num:Numeric[A]) {
		def tryToBigDecimal:Some[BigDecimal] = Num.tryToBigDecimal(self)
		def tryToBigInt:Option[BigInt] = Num.tryToBigInt(self)
		def tryToDouble:Option[Double] = Num.tryToDouble(self)
		def tryToFloat:Option[Float] = Num.tryToFloat(self)
		def tryToLong:Option[Long] = Num.tryToLong(self)
		def tryToInt:Option[Int] = Num.tryToInt(self)
	}
}
