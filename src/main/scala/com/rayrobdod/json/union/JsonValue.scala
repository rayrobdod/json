/*
	Copyright (c) 2015, Raymond Dodge
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

import com.rayrobdod.json.builder.Builder
import scala.language.implicitConversions

/**
 * A union type
 */
sealed trait JsonValue

object JsonValue {
	final case class JsonValueString(s:String) extends JsonValue
	final case class JsonValueNumber(i:Number) extends JsonValue
	final case class JsonValueBoolean(b:Boolean) extends JsonValue
	object JsonValueNull extends JsonValue
	final case class JsonValueByteStr(s:Array[Byte]) extends JsonValue {
		override def hashCode() = java.util.Arrays.hashCode(s)
		override def equals(other:Any) = other match {
			case JsonValueByteStr(other2) => java.util.Arrays.equals(s, other2)
			case _ => false
		}
	}
	
	implicit def apply(s:String):JsonValue = JsonValueString(s)
	implicit def apply(b:Boolean):JsonValue = JsonValueBoolean(b)
	implicit def apply(s:Array[Byte]):JsonValue = JsonValueByteStr(s)
	implicit def apply(i:Number):JsonValue = JsonValueNumber(i)
}
