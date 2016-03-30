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
import com.rayrobdod.json.parser.Parser
import scala.language.implicitConversions

/**
 * A union type where the possible values are a String or Int
 */
sealed trait StringOrInt

object StringOrInt {
	/** A string value represented as a StringOrInt union */
	final case class Left(s:String) extends StringOrInt
	/** An integer value represented as a StringOrInt union */
	final case class Right(i:Int) extends StringOrInt
	
	implicit def apply(s:String) = Left(s)
	implicit def apply(i:Int) = Right(i)
	
	
	
	final class FromStringKeyBuilder[V,A](inner:Builder[String,V,A]) extends Builder[StringOrInt,V,A] {
		def init:A = inner.init
		def apply[Input](key:StringOrInt):Function3[A, Input, Parser[StringOrInt, V, Input], A] = {(a,b,c) =>
			val strKey = key match {
				case Left(s) => s
				case Right(s) => s.toString
			}
			
			inner.apply(strKey).apply(a,b, new AsStringKeyParser(c))
		}
	}
	final class AsStringKeyParser[V,A](inner:Parser[StringOrInt,V,A]) extends Parser[String,V,A] {
		def parseComplex[Output](builder:Builder[String,V,Output], i:A):Output = inner.parseComplex(new FromStringKeyBuilder(builder), i)
		def parsePrimitive(i:A):V = inner.parsePrimitive(i)
	}
	
	final class AsStringKeyBuilder[V,A](inner:Builder[StringOrInt,V,A]) extends Builder[String,V,A] {
		def init:A = inner.init
		def apply[Input](key:String):Function3[A, Input, Parser[String, V, Input], A] = {(a,b,c) =>
			inner.apply(StringOrInt(key)).apply(a,b, new FromStringKeyParser(c))
		}
	}
	final class FromStringKeyParser[V,A](inner:Parser[String,V,A]) extends Parser[StringOrInt,V,A] {
		def parseComplex[Output](builder:Builder[StringOrInt,V,Output], i:A):Output = inner.parseComplex(new AsStringKeyBuilder(builder), i)
		def parsePrimitive(i:A):V = inner.parsePrimitive(i)
	}
}