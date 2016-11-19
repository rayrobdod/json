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
import com.rayrobdod.json.union.Numeric.NumericPair

/**
 * A union type representing primitive types in Json objects
 * @since 3.0
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
	final def fold[A](fs:String => A, fn:Numeric.NumericPair[_] => A, fb:Boolean => A, fz:Function0[A]):A = this match {
		case JsonValueString(s) => fs(s)
		case JsonValueNumber(n, t) => fn(Numeric.NumericPair(n)(t))
		case JsonValueBoolean(b) => fb(b)
		case JsonValueNull => fz.apply
	}
	
	/**
	 * Executes and returns `fs(this.s)` if this is a JsonValueString, else return a Left with an error message
	 */
	final def stringToEither[A](fs:String => Either[(String, Int),A]):Either[(String, Int),A] = {
		val unexpected = new ReturnLeft("Expected string")
		this.fold(fs, unexpected, unexpected, unexpected)
	}
	
	/**
	 * Executes and returns `fi(this.i)` if this is a JsonValueNumber which holds an number convertible to integer, else return a Left with an error message.
	 */
	final def integerToEither[A](fi:Int => Either[(String, Int),A]):Either[(String, Int),A] = {
		val number = {n:NumericPair[_] => n.tryToInt.fold[Either[(String, Int), A]](Left(("Expected integral number", 0))){fi}}
		val unexpected = new ReturnLeft("Expected integral number")
		this.fold(unexpected, number, unexpected, unexpected)
	}
	
	/**
	 * Executes and returns `fn(this.i)` if this is a JsonValueNumber, else return a Left with an error message.
	 */
	final def numberToEither[A](fn:Number => Either[(String, Int),A]):Either[(String, Int),A] = {
		val number = {n:NumericPair[_] => n.tryToBigDecimal.fold[Either[(String, Int),A]](Left(("Expected number", 0))){fn}}
		val unexpected = new ReturnLeft("Expected number")
		this.fold(unexpected, number, unexpected, unexpected)
	}
	
	/**
	 * Executes and returns `fb(this.b)` if this is a JsonValueBoolean, else return a Left with an error message
	 */
	final def booleanToEither[A](fb:Boolean => Either[(String, Int),A]):Either[(String, Int),A] = {
		val unexpected = new ReturnLeft("Expected boolean")
		this.fold(unexpected, unexpected, fb, unexpected)
	}
}

/**
 * The cases of JsonValue and methods to convert other things into JsonValues.
 * @since 3.0
 */
object JsonValue {
	final case class JsonValueString(s:String) extends JsonValue
	final case class JsonValueNumber[A](value:A, typ:Numeric[A]) extends JsonValue {
		override def equals(other:Any) = {
			if (other.isInstanceOf[JsonValueNumber[_]]) {
				val other2 = other.asInstanceOf[JsonValueNumber[_]]
				other2.tryToBigDecimal == this.tryToBigDecimal
			} else {false}
		}
		private def tryToBigDecimal:Option[BigDecimal] = typ.tryToBigDecimal(value)
	}
	final case class JsonValueBoolean(b:Boolean) extends JsonValue
	object JsonValueNull extends JsonValue {
		override def toString = "JsonValueNull"
	}
	
	implicit def apply(s:String):JsonValue = JsonValueString(s)
	implicit def apply(b:Boolean):JsonValue = JsonValueBoolean(b)
	implicit def apply[A](i:A)(implicit t:Numeric[A]):JsonValue = JsonValueNumber(i, t)
	
	
	
	/** Convert a StringOrInt value into a JsonValue */
	// Can't be called 'apply' as otherwise `JsonValue(x:Int)` confuses the compiler
	implicit def stringOrInt2JsonValue(s:StringOrInt):JsonValue = s match {
		case StringOrInt.Left(s) => JsonValueString(s)
		case StringOrInt.Right(i) => JsonValueNumber(i, Numeric.IntNumeric)
	}
	
	/** 
	 * Convert a CborValue into a JsonValue, where ByteStrs are instead converted into
	 * hexencoded strings.
	 */
	def cborValueHexencodeByteStr(x:CborValue):JsonValue = x match {
		case CborValue.CborValueString(s) => JsonValueString(s)
		case CborValue.CborValueBoolean(b) => JsonValueBoolean(b)
		case CborValue.CborValueNumber(n, t) => JsonValueNumber(n, t)
		case CborValue.CborValueByteStr(s) => JsonValueString(new String(
			s.flatMap{byte => ("00" + (0xFF & byte.intValue).toHexString).takeRight(2)}
		))
		case CborValue.CborValueNull => JsonValueNull
	}
	
	private class ReturnLeft(msg:String) extends Function1[Any, Either[(String, Int), Nothing]] with Function0[Either[(String, Int), Nothing]] {
		def apply():Either[(String, Int), Nothing] = Left(msg, 0)
		def apply(x:Any):Either[(String, Int), Nothing] = Left(msg, 0)
	}
}
