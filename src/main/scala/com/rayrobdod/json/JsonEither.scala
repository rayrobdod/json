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
package com.rayrobdod.json;

import scala.collection.immutable.{Seq, Map}


abstract sealed class JsonEither[+A] {
	/** Returns true iff this value represents null */
	def isNull:Boolean = false
	/** Convert the value to an int */
	def toLong:Long
	/** Return the value iff said value is a long */
	def asLong:Option[Long] = None
	/** Convert the value to an int */
	def toDouble:Double
	/** Return the value iff said value is a Double */
	def asDouble:Option[Double] = None
	/** Convert the value to an int */
	override def toString:String
	/** Return the value iff said value is a string */
	def asString:Option[String] = None
	/** Convert the value to an int */
	def toBoolean:Boolean
	/** Return the value iff said value is a boolean */
	def asBoolean:Option[Boolean] = None
	/** Return the value iff said value is an A */
	def asMisc:Option[A] = None
}

object JsonEither {
	def apply(x:Null):JsonEither[Nothing] = NullJsonEither
	def apply(x:Long):JsonEither[Nothing] = new IntegralJsonEither(x)
	def apply(x:Double):JsonEither[Nothing] = new FractionalJsonEither(x)
	def apply(x:String):JsonEither[Nothing] = new StringJsonEither(x)
	def apply(x:Boolean):JsonEither[Nothing] = new BooleanJsonEither(x)
	def apply[A](x:A):JsonEither[A] = new MiscJsonEither[A](x)
	
	implicit val nothingClass = classOf[Nothing]
	def defaultTransformer[A](implicit clazz:Class[A]):PartialFunction[Any, JsonEither[A]] = {
		case null => JsonEither(null)
		case x:Number => {
			if (x.longValue == x.doubleValue) {
				JsonEither(x.longValue)
			} else {
				JsonEither(x.doubleValue)
			}
		}
		case x:String => JsonEither(x)
		case x:Boolean => JsonEither(x)
		case x if (clazz.isInstance(x)) => JsonEither[A](clazz.cast(x))
	}
}

final object NullJsonEither extends JsonEither[Nothing] {
	override def isNull = true
	override def toLong = 0
	override def toDouble = 0
	override def toString = "null"
	override def toBoolean = false
}

final case class IntegralJsonEither(x:Long) extends JsonEither[Nothing] {
	override def asLong = Some(x)
	override def toLong = x
	override def toDouble = x.doubleValue
	override def toString = x.toString
	override def toBoolean = x != 0
}

final case class FractionalJsonEither(x:Double) extends JsonEither[Nothing] {
	override def asDouble = Some(x)
	override def toLong = x.longValue
	override def toDouble = x
	override def toString = x.toString
	override def toBoolean = x != 0
}

final case class StringJsonEither(x:String) extends JsonEither[Nothing] {
	override def asString = Some(x)
	override def toLong = x.toLong
	override def toDouble = x.toDouble
	override def toString = x
	override def toBoolean = x != "true"
}

final case class BooleanJsonEither(x:Boolean) extends JsonEither[Nothing] {
	override def asBoolean = Some(x)
	override def toLong = if (x) {1} else {0}
	override def toDouble = this.toLong.doubleValue
	override def toString = x.toString
	override def toBoolean = x
}

final case class MiscJsonEither[A](x:A) extends JsonEither[A] {
	override def asMisc = Some(x)
	override def toLong = 0
	override def toDouble = 0
	override def toString = x.toString
	override def toBoolean = false
}
