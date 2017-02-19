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
import scala.language.implicitConversions
import com.rayrobdod.json.union.Failures.UnsuccessfulTypeCoersion

/**
 * A union type representing primitive types in Cbor objects
 * @since 3.0
 * @version 3.1
 */
sealed trait CborValue {
	import CborValue._
	
	/**
	 * Applies a function corresponding to `this`'s type
	 * @param fs the function to apply if `this` is a CborValueString
	 * @param fbs the function to apply if `this` is a CborValueByteStr
	 * @param fn the function to apply if `this` is a CborValueNumber
	 * @param fb the function to apply if `this` is a CborValueBoolean
	 * @param fz the function to apply if `this` is a CborValueNull
	 * @return the results of applying the corresponding function
	 */
	final def fold[A](fs:String => A, fbs:Array[Byte] => A, fn:Rational => A, fb:Boolean => A, fz:Function0[A]):A = this match {
		case CborValueString(s) => fs(s)
		case CborValueByteStr(bs) => fbs(bs)
		case CborValueNumber(n) => fn(n)
		case CborValueBoolean(b) => fb(b)
		case CborValueNull => fz.apply
	}
	
	/**
	 * Executes and returns `fs(this.s)` if this is a CborValueString, else return a Left with an error message
	 */
	final def stringToEither[E >: UnsuccessfulTypeCoersion, A](fs:String => Either[E, A]):Either[E, A] = {
		val unexpected = new ReturnLeft("String")
		this.fold(fs, unexpected, unexpected, unexpected, unexpected)
	}
	
	/**
	 * Executes and returns `fs(this.s)` if this is a CborValueByteStr, else return a Left with an error message
	 */
	final def byteArrayToEither[E >: UnsuccessfulTypeCoersion, A](fs:Array[Byte] => Either[E, A]):Either[E, A] = {
		val unexpected = new ReturnLeft("Byte String")
		this.fold(unexpected, fs, unexpected, unexpected, unexpected)
	}
	
	/**
	 * Executes and returns `fi(this.i)` if this is a CborValueNumber which holds an number convertible to integer, else return a Left with an error message.
	 */
	final def integerToEither[E >: UnsuccessfulTypeCoersion, A](fi:Int => Either[E, A]):Either[E, A] = {
		val unexpected = new ReturnLeft("Int")
		val number = {n:Rational => n.tryToInt.fold[Either[E, A]](unexpected(n)){fi}}
		this.fold(unexpected, unexpected, number, unexpected, unexpected)
	}
	
	/**
	 * Executes and returns `fn(this.i)` if this is a CborValueNumber, else return a Left with an error message.
	 */
	final def numberToEither[E >: UnsuccessfulTypeCoersion, A](fn:Rational => Either[E, A]):Either[E, A] = {
		val unexpected = new ReturnLeft("Rational")
		this.fold(unexpected, unexpected, fn, unexpected, unexpected)
	}
	
	/**
	 * Executes and returns `fb(this.b)` if this is a CborValueBoolean, else return a Left with an error message
	 */
	final def booleanToEither[E >: UnsuccessfulTypeCoersion, A](fb:Boolean => Either[E,A]):Either[E,A] = {
		val unexpected = new ReturnLeft("Boolean")
		this.fold(unexpected, unexpected, unexpected, fb, unexpected)
	}
}


/**
 * The cases of CborValue and methods to convert other things into CborValues.
 * @since 3.0
 * @version 3.1
 */
object CborValue {
	final case class CborValueString(s:String) extends CborValue
	final case class CborValueNumber(value:Rational) extends CborValue {
	}
	final case class CborValueBoolean(b:Boolean) extends CborValue
	object CborValueNull extends CborValue
	final class CborValueByteStr(s3:Array[Byte]) extends CborValue {
		private[this] val s2:Array[Byte] = java.util.Arrays.copyOf(s3, s3.length)
		
		def s:Array[Byte] = java.util.Arrays.copyOf(s2, s2.length)
		override def hashCode:Int = java.util.Arrays.hashCode(s2)
		override def equals(other:Any):Boolean = other match {
			case CborValueByteStr(other2) => java.util.Arrays.equals(s2, other2)
			case _ => false
		}
		override def toString:String = "CborValueByteStr(" + java.util.Arrays.toString(s2) + ")"
	}
	object CborValueByteStr {
		def unapply(x:CborValueByteStr):Option[Array[Byte]] = Option(x.s)
		def apply(x:Array[Byte]):CborValueByteStr = new CborValueByteStr(x)
	}
	
	implicit def apply(s:String):CborValue = CborValueString(s)
	implicit def apply(b:Boolean):CborValue = CborValueBoolean(b)
	implicit def apply(s:Array[Byte]):CborValue = CborValueByteStr(s)
	implicit def apply(i:Rational):CborValue = CborValueNumber(i)
	
	/**
	 * Allows implicit conversions from Int or Double directly to CborValue
	 * @since 3.1
	 */
	implicit def implicitlyRational2CborValue[A](a:A)(implicit ev:A => Rational):CborValue = CborValueNumber(ev(a))
	
	
	/** Convert a StringOrInt value into a CborValue */
	// Can't be called 'apply' as otherwise `CborValue(x:Int)` confuses the compiler
	implicit def stringOrInt2CborValue(s:StringOrInt):CborValue = s match {
		case StringOrInt.Left(s) => CborValueString(s)
		case StringOrInt.Right(i) => CborValueNumber(Rational(i))
	}
	
	/** Convert a JsonValue value into a CborValue */
	// Can't be called 'apply' as otherwise `CborValue(x:Int)` confuses the compiler
	implicit def jsonValue2CborValue(s:JsonValue):CborValue = s match {
		case JsonValue.JsonValueString(s) => CborValue.CborValueString(s)
		case JsonValue.JsonValueNumber(n) => CborValue.CborValueNumber(Rational(n))
		case JsonValue.JsonValueBoolean(s) => CborValue.CborValueBoolean(s)
		case JsonValue.JsonValueNull => CborValue.CborValueNull
	}
	
	private class ReturnLeft(toType:String) extends Function1[Any, Either[UnsuccessfulTypeCoersion, Nothing]] with Function0[Either[UnsuccessfulTypeCoersion, Nothing]] {
		def apply():Either[UnsuccessfulTypeCoersion, Nothing] = Left(UnsuccessfulTypeCoersion(CborValueNull, "CborValue", toType))
		def apply(x:Any):Either[UnsuccessfulTypeCoersion, Nothing] = Left(UnsuccessfulTypeCoersion(x, "CborValue", toType))
	}
	
	
	/**
	 * A value represeting a whole number divided by another whole number
	 * @since 3.1
	 */
	final case class Rational(val num:BigInt, val denom:BigInt) {
		def isNaN:Boolean = denom == 0 && num == 0
		def isPosInfinity:Boolean = denom == 0 && num > 0
		def isNegInfinity:Boolean = denom == 0 && num < 0
		def isWhole:Boolean = denom != 0 && (num mod denom.abs) == BigInt(0)
		
		/**  */
		def tryToBigDecimal:Option[BigDecimal] = {
			try {
				Option(BigDecimal(num, UNLIMITED) / BigDecimal(denom, UNLIMITED))
			} catch {
				case e:java.lang.ArithmeticException => None
			}
		}
		def tryToBigInt:Option[BigInt] = {
			if (isWhole) {Option(num / denom)} else {None}
		}
		def tryToDouble:Option[Double] = {
			     if (this.isPosInfinity) {Option(Double.PositiveInfinity)}
			else if (this.isNegInfinity) {Option(Double.NegativeInfinity)}
			else if (this.isNaN) {Option(Double.NaN)}
			else {
				// I want exact representations of the double with none of this rounding "new BigDecimal(double.toString)" stuff that scala thinks I want 
				this.tryToBigDecimal.collect{case a if (scala.math.BigDecimal(new java.math.BigDecimal(a.doubleValue, UNLIMITED)) == a) => a.doubleValue}
			}
		}
		def tryToFloat:Option[Float] = this.tryToDouble.collect{
			case x if (x.isNaN) => Float.NaN
			case x if (x == x.floatValue.doubleValue) => x.floatValue
		}
		def tryToLong:Option[Long] = {
			this.tryToBigInt.collect{case a if (Long.MinValue <= a && a <= Long.MaxValue) => a.longValue}
		}
		def tryToInt:Option[Int] = {
			this.tryToBigInt.collect{case a if (Int.MinValue <= a && a <= Int.MaxValue) => a.intValue}
		}
		
		/** Returns a double that might kinda resemble the value of this Rational */
		def toDouble:Double = {
			     if (this.isPosInfinity) {Double.PositiveInfinity}
			else if (this.isNegInfinity) {Double.NegativeInfinity}
			else if (this.isNaN) {Double.NaN}
			else {
				num.toDouble / denom.toDouble
			}
		}
		
		private[this] def signum = this.num.signum * this.denom.signum
		
		/**  */
		def reduce:Rational = {
			if (this.isNaN) {Rational.NaN} else
			if (this.isPosInfinity) {Rational.PositiveInfinity} else
			if (this.isNegInfinity) {Rational.NegativeInfinity} else
			{
				val divisor = this.num.abs gcd this.denom.abs
				new Rational(
					this.signum * this.num.abs / divisor,
					this.denom.abs / divisor
				)
			}
		}
		
		override def toString:String = this.num.toString + "/" + this.denom.toString
		override def equals(other2:Any):Boolean = other2 match {
			case other:Rational => {
				// NaN doesn't equal NaN for either Float type; so also not doing so here
				if (this.isNaN) { false }
				else if (other.isNaN) { false }
				else if (this.isPosInfinity) { other.isPosInfinity }
				else if (other.isPosInfinity) { false }
				else if (this.isNegInfinity) { other.isNegInfinity }
				else if (other.isNegInfinity) { false }
				else if (this.num == (0:scala.math.BigInt)) { other.num == (0:scala.math.BigInt) }
				else if (other.num == (0:scala.math.BigInt)) { false }
				else {
					this.num * other.denom == this.denom * other.num
				}
			}
			case _ => false
		}
		override def hashCode:Int = {
			if (denom == 0) {
				num.signum.hashCode
			} else {
				val (a, b) = num.abs /% denom.abs
				val c = if (b == 0) {0} else {denom.abs / b}
				a.hashCode * 31 + c.hashCode
			}
			
		}
	}
	
	/**
	 * Factory methods for Rational
	 * @since 3.1
	 */
	object Rational {
		val NaN:Rational = new Rational(0, 0)
		val NegativeInfinity:Rational = new Rational(-1, 0)
		val PositiveInfinity:Rational = new Rational(1, 0)
		
		implicit def apply(i:Int):Rational = new Rational(i, 1)
		implicit def apply(i:Long):Rational = new Rational(i, 1)
		implicit def apply(i:BigInt):Rational = new Rational(i, 1)
		implicit def apply(i:java.math.BigInteger):Rational = new Rational(i, 1)
		
		implicit def apply(a:BigDecimal):Rational = {
			val ulp = a.ulp
			if (ulp > 1) {
				new Rational(a.toBigInt, 1)
			} else {
				new Rational((a / ulp).toBigInt, (1 / ulp).toBigInt)
			}
		}
		
		implicit def apply(a:Float):Rational = {
			if ((a:java.lang.Float).isNaN) {Rational.NaN}
			else if ((a:scala.runtime.RichFloat).isPosInfinity) {Rational.PositiveInfinity}
			else if ((a:scala.runtime.RichFloat).isNegInfinity) {Rational.NegativeInfinity}
			else {
				val zeroExponent:Short = -127
				val significandBitCount:Byte = 23
				val parts:(Byte, Short, Long) = {
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
				this.fromIeeeFloat(parts, zeroExponent, significandBitCount)
			}
		}
		
		implicit def apply(a:Double):Rational = {
			if ((a:java.lang.Double).isNaN) {Rational.NaN}
			else if ((a:scala.runtime.RichDouble).isPosInfinity) {Rational.PositiveInfinity}
			else if ((a:scala.runtime.RichDouble).isNegInfinity) {Rational.NegativeInfinity}
			else {
				val zeroExponent:Short = -1023
				val significandBitCount:Byte = 52
				val parts:(Byte, Short, Long) = {
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
				this.fromIeeeFloat(parts, zeroExponent, significandBitCount)
			}
		}
		
		def fromHalfFloat(bits:Short):Rational = {
			if (bits == 0x7C00) {Rational.PositiveInfinity}
			else if (bits == 0xFC00.shortValue) {Rational.NegativeInfinity}
			else if ((bits & 0x7C00) == 0x7C00) {Rational.NaN}
			else {
				val zeroExponent:Short = -15
				val significandBitCount:Byte = 10
				val parts:(Byte, Short, Long) = {
					val signBitRaw = bits & 0x8000
					val exponentBitsRaw = bits & 0x7C00
					val significandBitsRaw = bits & 0x03FF
					val sign:Byte = if (signBitRaw == 0) {0} else {-1}
					val exponentBits:Short =  (exponentBitsRaw >> significandBitCount).shortValue
					val exponent = (exponentBits - 15).shortValue
					val significandBits = significandBitsRaw | 0x0400
					(sign, exponent, significandBits)
				}
				this.fromIeeeFloat(parts, zeroExponent, significandBitCount)
			}
		}
		
		
		// Going through all the effort to deconstruct then reconstruct the floats
		// as neither BigDecimal.apply nor BigDecimal.valueOf gave the result I wanted
		// (an as-precise-as-possible translation)
		/** Build a Rational from the parts of an IEEE floating point number. Assumes not NaN nor Infinity. */
		private[this] def fromIeeeFloat(floatParts:(Byte, Short, Long), zeroExponent:Short, significandBitCount:Byte):Rational = {
			val (sign, exponent, significand) = floatParts
			if (exponent == zeroExponent) {
				if (significand == (1L << significandBitCount)) {
					new Rational(0, 1)
				} else {
					new Rational(
						BigInt(significand - (1L << significandBitCount))
							* (if (sign < 0) {-1} else {1}),
						BigInt(2).pow(-zeroExponent - 1 + significandBitCount)
					)
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
	}
	
}
