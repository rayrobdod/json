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

import scala.language.implicitConversions
import com.rayrobdod.json.builder.PiecewiseBuilder.Failures
import com.rayrobdod.json.builder.PiecewiseBuilder.Failures.UnsuccessfulTypeCoercion

/**
 * A union type representing primitive types in Json objects
 * @since 3.0
 * @version 4.0
 */
sealed trait JsonValue {
	import JsonValue._
	
	/**
	 * Applies a function corresponding to `this`'s type
	 * @param fs the function to apply if `this` is a JsonValueString
	 * @param fn the function to apply if `this` is a JsonValueNumber
	 * @param fb the function to apply if `this` is a JsonValueBoolean
	 * @param fz the function to apply if `this` is a JsonValueNull
	 * @return the results of applying the corresponding function
	 */
	final def fold[A](fs:String => A, fn:BigDecimal => A, fb:Boolean => A, fz:Function0[A]):A = this match {
		case JsonValueString(s) => fs(s)
		case JsonValueNumber(n) => fn(n)
		case JsonValueBoolean(b) => fb(b)
		case JsonValueNull => fz.apply
	}
	
	/**
	 * Executes `then(this.s)` if this is a JsonValueString; otherwise executes `else(this)`
	 * @since 4.0
	 */
	final def ifIsString[A](`then`:String => A, `else`:JsonValue => A):A = this match {
		case JsonValueString(s) => `then`(s)
		case v => `else`(v)
	}
	
	/**
	 * Executes and returns `fs(this.s)` if this is a JsonValueString, else return a Left with an error message
	 */
	final def stringToEither[A](fs:String => Either[Failures, A]):Either[Failures, A] = {
		this.ifIsString(fs, {x => Left(UnsuccessfulTypeCoercion)})
	}
	
	/**
	 * Executes `then(this.s)` if this is a JsonValueNumber containing an Int; otherwise executes `else(this)`
	 * @since 4.0
	 */
	final def ifIsInteger[A](`then`:Int => A, `else`:JsonValue => A):A = this match {
		case JsonValueNumber(n) => {
			if (n.isValidInt) {
				`then`(n.intValue)
			} else {
				`else`( JsonValueNumber(n) )
			}
		}
		case v => `else`(v)
	}
	
	/**
	 * Executes and returns `fi(this.i)` if this is a JsonValueNumber which holds an number convertible to integer, else return a Left with an error message.
	 */
	final def integerToEither[A](fi:Int => Either[Failures, A]):Either[Failures, A] = {
		this.ifIsInteger(fi, {x => Left(UnsuccessfulTypeCoercion)})
	}
	
	/**
	 * Executes `then(this.s)` if this is a JsonValueNumber; otherwise executes `else(this)`
	 * @since 4.0
	 */
	final def ifIsNumber[A](`then`:BigDecimal => A, `else`:JsonValue => A):A = this match {
		case JsonValueNumber(n) => `then`(n)
		case v => `else`(v)
	}
	
	/**
	 * Executes and returns `fn(this.i)` if this is a JsonValueNumber, else return a Left with an error message.
	 */
	final def numberToEither[A](fn:BigDecimal => Either[Failures, A]):Either[Failures, A] = {
		this.ifIsNumber(fn, {x => Left(UnsuccessfulTypeCoercion)})
	}
	
	/**
	 * Executes `then(this.s)` if this is a JsonValueBoolean; otherwise executes `else(this)`
	 * @since 4.0
	 */
	final def ifIsBoolean[A](`then`:Boolean => A, `else`:JsonValue => A):A = this match {
		case JsonValueBoolean(b) => `then`(b)
		case v => `else`(v)
	}
	
	/**
	 * Executes and returns `fb(this.b)` if this is a JsonValueBoolean, else return a Left with an error message
	 */
	final def booleanToEither[A](fb:Boolean => Either[Failures, A]):Either[Failures, A] = {
		this.ifIsBoolean(fb, {x => Left(UnsuccessfulTypeCoercion)})
	}
}

/**
 * The cases of JsonValue and methods to convert other things into JsonValues.
 * @since 3.0
 */
object JsonValue {
	final case class JsonValueString(s:String) extends JsonValue
	final case class JsonValueNumber(value:BigDecimal) extends JsonValue {
	}
	final case class JsonValueBoolean(b:Boolean) extends JsonValue
	object JsonValueNull extends JsonValue {
		override def toString:String = "JsonValueNull"
	}
	
	implicit def apply(s:String):JsonValue = JsonValueString(s)
	implicit def apply(b:Boolean):JsonValue = JsonValueBoolean(b)
	implicit def apply(i:BigDecimal):JsonValue = JsonValueNumber(i)
	
	/**
	 * Allows implicit conversions from Int or Double directly to JsonValue
	 * @since 3.1
	 */
	implicit def implicitlyBigDecimal2JsonValue[A](a:A)(implicit ev:A => BigDecimal):JsonValue = JsonValueNumber(ev(a))
	
	
	/** Convert a StringOrInt value into a JsonValue */
	// Can't be called 'apply' as otherwise `JsonValue(x:Int)` confuses the compiler
	implicit def stringOrInt2JsonValue(s:StringOrInt):JsonValue = s match {
		case StringOrInt.Left(s) => JsonValueString(s)
		case StringOrInt.Right(i) => JsonValueNumber(i)
	}
	
	/** 
	 * Convert a CborValue into a JsonValue, where ByteStrs are instead converted into
	 * hexencoded strings.
	 */
	@deprecated("doesn't handle incompatible numbers; use cborValue2JsonValueEither then fold that method's return value", "3.1")
	def cborValueHexencodeByteStr(x:CborValue):JsonValue = x match {
		case CborValue.CborValueString(s) => JsonValueString(s)
		case CborValue.CborValueBoolean(b) => JsonValueBoolean(b)
		case CborValue.CborValueNumber(r) => JsonValueNumber(r.tryToBigDecimal.get)
		case CborValue.CborValueByteStr(s) => JsonValueString(new String(
			s.flatMap{byte => ("00" + (0xFF & byte.intValue).toHexString).takeRight(2)}
		))
		case CborValue.CborValueNull => JsonValueNull
	}
	
	/** 
	 * Convert a CborValue into a JsonValue if there is an equivalent JsonValue; else return a UnsuccessfulTypeCoercion.
	 * @since 3.1
	 */
	def cborValue2JsonValueEither(x:CborValue):Either[Either[Array[Byte], CborValue.Rational], JsonValue] = x match {
		case CborValue.CborValueString(s) => Right(JsonValueString(s))
		case CborValue.CborValueBoolean(b) => Right(JsonValueBoolean(b))
		case CborValue.CborValueNumber(r) => r.tryToBigDecimal.fold[Either[Either[Array[Byte], CborValue.Rational], JsonValue]](Left(Right(r))){x => Right(JsonValueNumber(x))}
		case CborValue.CborValueByteStr(s) => Left(Left(s))
		case CborValue.CborValueNull => Right(JsonValueNull)
	}
	
}
