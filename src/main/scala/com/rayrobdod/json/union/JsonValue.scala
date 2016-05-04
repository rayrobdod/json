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

import com.rayrobdod.json.parser.Parser
import com.rayrobdod.json.builder.Builder
import scala.language.implicitConversions

/**
 * A union type
 * @since next
 */
sealed trait JsonValue

/**
 * Methods used to create JsonValues
 * @since next
 */
object JsonValue {
	final case class JsonValueString(s:String) extends JsonValue
	final case class JsonValueNumber(i:Number) extends JsonValue
	final case class JsonValueBoolean(b:Boolean) extends JsonValue
	object JsonValueNull extends JsonValue
	final case class JsonValueByteStr(s:Array[Byte]) extends JsonValue {
		override def hashCode:Int = java.util.Arrays.hashCode(s)
		override def equals(other:Any):Boolean = other match {
			case JsonValueByteStr(other2) => java.util.Arrays.equals(s, other2)
			case _ => false
		}
	}
	
	implicit def apply(s:String):JsonValue = JsonValueString(s)
	implicit def apply(b:Boolean):JsonValue = JsonValueBoolean(b)
	implicit def apply(s:Array[Byte]):JsonValue = JsonValueByteStr(s)
	implicit def apply(i:Number):JsonValue = JsonValueNumber(i)
	
	
	
	/** Convert a StringOrInt value intoa JsonValue */
	// Can't be called 'apply' as otherwise `JsonValue(x:Int)` confuses the compiler
	implicit def stringOrInt2JsonValue(s:StringOrInt):JsonValue = s match {
		case StringOrInt.Left(s) => JsonValueString(s)
		case StringOrInt.Right(i) => JsonValueNumber(i)
	}
	
	/** Unwraps a JsonValue and returns whatever was inside */
	def unwrap(x:JsonValue):Any = x match {
		case JsonValueString(s) => s
		case JsonValueBoolean(b) => b
		case JsonValueNumber(b) => b
		case JsonValueByteStr(s) => s
		case JsonValueNull => null
	}
	
	/**
	 * Attempts to wrap a value inside a JsonValue.
	 * @throws MatchError if 
	 */
	def unsafeWrap(x:Any):JsonValue = x match {
		case s:String => JsonValueString(s)
		case b:Number => JsonValueNumber(b)
		case b:Boolean => JsonValueBoolean(b)
	}
}
