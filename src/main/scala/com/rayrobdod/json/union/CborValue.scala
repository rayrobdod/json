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

/**
 * A union type representing primitive types in Cbor objects
 * @since 3.0
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
	final def fold[A](fs:String => A, fbs:Array[Byte] => A, fn:Number => A, fb:Boolean => A, fz:Function0[A]):A = this match {
		case CborValueString(s) => fs(s)
		case CborValueByteStr(bs) => fbs(bs)
		case CborValueNumber(n) => fn(n)
		case CborValueBoolean(b) => fb(b)
		case CborValueNull => fz.apply
	}
	
	/**
	 * Executes and returns `fs(this.s)` if this is a CborValueString, else return a Left with an error message
	 */
	final def stringToEither[A](fs:String => Either[(String, Int),A]):Either[(String, Int),A] = {
		val unexpected = new ReturnLeft("Expected string")
		this.fold(fs, unexpected, unexpected, unexpected, unexpected)
	}
	
	/**
	 * Executes and returns `fs(this.s)` if this is a CborValueByteStr, else return a Left with an error message
	 */
	final def byteArrayToEither[A](fs:Array[Byte] => Either[(String, Int),A]):Either[(String, Int),A] = {
		val unexpected = new ReturnLeft("Expected string")
		this.fold(unexpected, fs, unexpected, unexpected, unexpected)
	}
	
	/**
	 * Executes and returns `fi(this.i)` if this is a CborValueNumber which holds an number convertible to integer, else return a Left with an error message.
	 * 
	 * I somewhat doubt this method's ability to deal with numbers more precise than doubles can handle, but there is no Number -> BigFloat function. 
	 */
	final def integerToEither[A](fi:Int => Either[(String, Int),A]):Either[(String, Int),A] = {
		val number = {n:Number => if (n.intValue.doubleValue == n.doubleValue) {fi(n.intValue)} else {Left("Expected integral number", 0)}} 
		val unexpected = new ReturnLeft("Expected integral number")
		this.fold(unexpected, unexpected, number, unexpected, unexpected)
	}
	
	/**
	 * Executes and returns `fn(this.i)` if this is a CborValueNumber, else return a Left with an error message.
	 */
	final def numberToEither[A](fn:Number => Either[(String, Int),A]):Either[(String, Int),A] = {
		val unexpected = new ReturnLeft("Expected number")
		this.fold(unexpected, unexpected, fn, unexpected, unexpected)
	}
	
	/**
	 * Executes and returns `fb(this.b)` if this is a CborValueBoolean, else return a Left with an error message
	 */
	final def booleanToEither[A](fb:Boolean => Either[(String, Int),A]):Either[(String, Int),A] = {
		val unexpected = new ReturnLeft("Expected boolean")
		this.fold(unexpected, unexpected, unexpected, fb, unexpected)
	}
}


/**
 * The cases of CborValue and methods to convert other things into CborValues.
 * @since 3.0
 */
object CborValue {
	final case class CborValueString(s:String) extends CborValue
	final case class CborValueNumber(i:Number) extends CborValue
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
	implicit def apply(i:Number):CborValue = CborValueNumber(i)
	
	
	/** Convert a StringOrInt value intoa CborValue */
	// Can't be called 'apply' as otherwise `CborValue(x:Int)` confuses the compiler
	implicit def stringOrInt2CborValue(s:StringOrInt):CborValue = s match {
		case StringOrInt.Left(s) => CborValueString(s)
		case StringOrInt.Right(i) => CborValueNumber(i)
	}
	
	/** Convert a JsonValue value into a CborValue */
	// Can't be called 'apply' as otherwise `CborValue(x:Int)` confuses the compiler
	implicit def jsonValue2CborValue(s:JsonValue):CborValue = s match {
		case JsonValue.JsonValueString(s) => CborValue.CborValueString(s)
		case JsonValue.JsonValueNumber(s) => CborValue.CborValueNumber(s)
		case JsonValue.JsonValueBoolean(s) => CborValue.CborValueBoolean(s)
		case JsonValue.JsonValueNull => CborValue.CborValueNull
	}
	
	/** Unwraps a CborValue and returns whatever was inside */
	def unwrap(x:CborValue):Any = x match {
		case CborValueString(s) => s
		case CborValueBoolean(b) => b
		case CborValueNumber(b) => b
		case CborValueByteStr(s) => s
		case CborValueNull => null
	}
	
	/**
	 * Attempts to wrap a value inside a CborValue.
	 * @throws MatchError if 
	 */
	def unsafeWrap(x:Any):CborValue = x match {
		case s:String => CborValueString(s)
		case b:Number => CborValueNumber(b)
		case b:Boolean => CborValueBoolean(b)
		case ab:Array[Byte] => CborValueByteStr(ab)
	}
	
	private class ReturnLeft(msg:String) extends Function1[Any, Either[(String, Int), Nothing]] with Function0[Either[(String, Int), Nothing]] {
		def apply():Either[(String, Int), Nothing] = Left(msg, 0)
		def apply(x:Any):Either[(String, Int), Nothing] = Left(msg, 0)
	}
}
