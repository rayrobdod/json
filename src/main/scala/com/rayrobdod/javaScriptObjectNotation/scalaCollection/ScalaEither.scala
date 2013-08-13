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
