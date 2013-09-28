/*
	Copyright (c) 2012-2013, Raymond Dodge
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
package com.rayrobdod.javaScriptObjectNotation.scalaCollection

import scala.collection.immutable.{Map => SMap, Seq => SSeq}

/**
 * A class which represents any possible value that can be in a JSON entity.
 * 
 * This is looking suspiciously close to the `JSONString extends JSONValue` thing that I dislike
 * @version 2013 Jun 21
 */
sealed abstract class JSONEither /* extends scala.Proxy */ {
	def toMap:SMap[String, JSONEither]
	def toSeq:SSeq[JSONEither]
	def toNumber:Number
	def toBool:Boolean
	def toString:String
	def isNull:Boolean = false
	
	def toInt:Int = this.toNumber.intValue
	def toDouble:Double = this.toNumber.doubleValue
}

/*
trait BSONEither extends JSONEither {
	def toDatetime:java.util.Date
	def toRegEx:java.util.regex.Pattern
	def isMin:Boolean = false
	def isMax:Boolean = false
}*/

/**
 * @version 2013 Jun 21
 */
object JSONEither {
	private class NumberJSONEither(val self:Number) extends JSONEither {
		override def toMap = throw new ClassCastException("Cannot turn Number into Map")
		override def toSeq = this :: Nil
		override def toNumber = self
		override def toBool = throw new ClassCastException("Cannot turn Number into Bool")
		override def toString = self.toString
	}
	private class StringJSONEither(val self:String) extends JSONEither {
		override def toMap = throw new ClassCastException("Cannot turn String into Map")
		override def toSeq = this :: Nil
		override def toNumber = throw new ClassCastException("Cannot turn String into Number")
		override def toBool = throw new ClassCastException("Cannot turn String into Bool")
		override def toString = self
	}
	private class BooleanJSONEither(val self:Boolean) extends JSONEither {
		override def toMap = throw new ClassCastException("Cannot turn Bool into Map")
		override def toSeq = this :: Nil
		override def toNumber = throw new ClassCastException("Cannot turn Bool into Number")
		override def toBool = self
		override def toString = self.toString
	}
	private class SeqJSONEither(val self:SSeq[JSONEither]) extends JSONEither {
		override def toMap = throw new ClassCastException("Cannot turn Seq into Map")
		override def toSeq = self
		override def toNumber = throw new ClassCastException("Cannot turn Seq into Number")
		override def toBool = throw new ClassCastException("Cannot turn Seq into Bool")
		override def toString = self.toString
	}
	private class MapJSONEither(val self:SMap[String,JSONEither]) extends JSONEither {
		override def toMap = self
		override def toSeq = throw new ClassCastException("Cannot turn Map into Seq")
		override def toNumber = throw new ClassCastException("Cannot turn Map into Number")
		override def toBool = throw new ClassCastException("Cannot turn Map into Bool")
		override def toString = self.toString
	}
	private object NullJSONEither extends JSONEither {
		val self = null
		
		override def toMap = throw new ClassCastException("Cannot convert null to anything")
		override def toSeq = throw new ClassCastException("Cannot convert null to anything")
		override def toNumber = throw new ClassCastException("Cannot convert null to anything")
		override def toBool = throw new ClassCastException("Cannot convert null to anything")
		override def toString = "null"
		override def isNull = true
	}
	
	
	def apply(x:Number):JSONEither = new NumberJSONEither(x)
	def apply(x:String):JSONEither = new StringJSONEither(x)
	def apply(x:Boolean):JSONEither = new BooleanJSONEither(x)
	def apply(x:SSeq[JSONEither]):JSONEither = new SeqJSONEither(x)
	def apply(x:SMap[String,JSONEither]):JSONEither = new MapJSONEither(x)
	def getNull:JSONEither = NullJSONEither
}
