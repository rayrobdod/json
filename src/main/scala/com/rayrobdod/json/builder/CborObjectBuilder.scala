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
package com.rayrobdod.json.builder;

import scala.collection.immutable.Seq
import java.nio.charset.StandardCharsets.UTF_8
import com.rayrobdod.json.parser.CborParser.{MajorTypeCodes, SimpleValueCodes}
import com.rayrobdod.json.union.{JsonValue, JsonValueOrCollection}
import com.rayrobdod.json.parser.Parser
import com.rayrobdod.json.parser.{MapParser, SeqParser}

/**
 * A builder that will create a series of bytes in Cbor Object format
 * @constructor
 * A builder that will create cbor object format byte strings
 * @param transformer a function to convert non-cbor-primitive objects to cbor-primitive objects
 */
// TODO: widen key to include everything else a CBOR key can be
final class CborObjectBuilder extends Builder[JsonValue, JsonValueOrCollection, Seq[Byte]] {
	import CborObjectBuilder._
	
	val init:Seq[Byte] = encodeLength(MajorTypeCodes.OBJECT, 0)
	
	/** @param folding a valid cbor object */
	def apply[Input](key:JsonValue):Function3[Seq[Byte], Input, Parser[JsonValue, JsonValueOrCollection, Input], Seq[Byte]] = {(folding, input, parser) =>
		val value = parser.parsePrimitive(input)
		
		val headerByte:Byte = folding.head
		val additionalInfo = headerByte & 0x1F
		val (objectLength:Long, passData:Seq[Byte]) = additionalInfo match {
			case x if (x <= 23) => { (x.longValue, folding.tail) }
			case 24 => { (folding(1).longValue & 0xFF, folding.tail.tail) }
			case 25 => { (byteArray2Long(folding.drop(1).take(2)), folding.drop(3)) }
			case 26 => { (byteArray2Long(folding.drop(1).take(4)), folding.drop(5)) }
			case 27 => { (byteArray2Long(folding.drop(1).take(8)), folding.drop(9)) }
			case _  => {throw new IllegalArgumentException("input `folding` had illegal length value")}
		}
		
		encodeLength(MajorTypeCodes.OBJECT, objectLength + 1) ++ passData ++ encodeValue(key) ++ encodeValue(value)
	}
}

/**
 * A builder that will create a series of bytes in Cbor Array format
 * @constructor
 * A builder that will create cbor array format byte strings
 * @param transformer a function to convert non-cbor-primitive objects to cbor-primitive objects
 */
final class CborArrayBuilder() extends Builder[Any, JsonValueOrCollection, Seq[Byte]] {
	import CborObjectBuilder._
	
	val init:Seq[Byte] = encodeLength(MajorTypeCodes.ARRAY, 0)
	
	/** @param folding a valid cbor object */
	def apply[Input](key:Any):Function3[Seq[Byte], Input, Parser[Any, JsonValueOrCollection, Input], Seq[Byte]] = {(folding, input, parser) =>
		val value = parser.parsePrimitive(input)
		
		val headerByte:Byte = folding.head
		val additionalInfo = headerByte & 0x1F
		val (objectLength:Long, passData:Seq[Byte]) = additionalInfo match {
			case x if (x <= 23) => { (x.longValue, folding.tail) }
			case 24 => { (folding(1).longValue & 0xFF, folding.tail.tail) }
			case 25 => { (byteArray2Long(folding.drop(1).take(2)), folding.drop(3)) }
			case 26 => { (byteArray2Long(folding.drop(1).take(4)), folding.drop(5)) }
			case 27 => { (byteArray2Long(folding.drop(1).take(8)), folding.drop(9)) }
			case _  => {throw new IllegalArgumentException("input `folding` had illegal length value")}
		}
		
		encodeLength(MajorTypeCodes.ARRAY, objectLength + 1) ++ passData ++ encodeValue(value)
	}
}

private[builder] object CborObjectBuilder {
	import com.rayrobdod.json.union.JsonValue._
	import com.rayrobdod.json.union.JsonValueOrCollection._
	
	/**
	 * Encode a number into CBOR form.
	 * @param majorType the major type to prepend to the number. 0 ≥ x ≥ 7
	 * @param value the value to encode
	 */
	private[builder] def encodeLength(majorType:Byte, value:Long):Seq[Byte] = {
		val majorTypeShifted:Byte = (majorType << 5).byteValue
		
		val (headerByte:Int, rest:Seq[Byte]) = value match {
			case x:Long if (x < 0)           => throw new IllegalArgumentException("Value may not be negative")
			case x:Long if (x <= 23)         => ((majorTypeShifted + x.intValue,  Seq.empty[Byte]))
			case x:Long if (x <= 0xFF)       => ((majorTypeShifted + 24, long2ByteArray(x, 1)))
			case x:Long if (x <= 0xFFFF)     => ((majorTypeShifted + 25, long2ByteArray(x, 2)))
			case x:Long if (x <= 0xFFFFFFFFL) => ((majorTypeShifted + 26, long2ByteArray(x, 4)))
			case x:Long                      => ((majorTypeShifted + 27, long2ByteArray(x, 8)))
		}
		
		headerByte.byteValue +: rest
	}
	
	private[builder] def encodeValue(v:JsonValueOrCollection):Seq[Byte] = v match {
		case JVCValue(JsonValueBoolean(false)) => encodeLength(MajorTypeCodes.SPECIAL, SimpleValueCodes.FALSE)
		case JVCValue(JsonValueBoolean(true))  => encodeLength(MajorTypeCodes.SPECIAL, SimpleValueCodes.TRUE)
		case JVCValue(JsonValueNull)  => encodeLength(MajorTypeCodes.SPECIAL, SimpleValueCodes.NULL)
		case JVCValue(JsonValueNumber(x:java.lang.Float)) => Seq((0xE0 + SimpleValueCodes.FLOAT).byteValue) ++ long2ByteArray(java.lang.Float.floatToIntBits(x), 4)
		case JVCValue(JsonValueNumber(x:java.lang.Double)) => Seq((0xE0 + SimpleValueCodes.DOUBLE).byteValue) ++ long2ByteArray(java.lang.Double.doubleToLongBits(x))
		case JVCValue(JsonValueString(x:String)) => {
			val bytes = x.getBytes(UTF_8)
			encodeLength(MajorTypeCodes.STRING, bytes.length) ++ bytes
		}
		case JVCValue(JsonValueNumber(x:Number)) if (x.longValue >= 0) => encodeLength(MajorTypeCodes.POSITIVE_INT, x.longValue)
		case JVCValue(JsonValueNumber(x:Number)) if (x.longValue < 0) => encodeLength(MajorTypeCodes.NEGATIVE_INT, -1 - x.longValue)
		case JVCValue(JsonValueByteStr(bytes:Array[Byte])) => encodeLength(MajorTypeCodes.BYTE_ARRAY, bytes.length) ++ bytes
		case JVCMap(x:Map[String, JsonValueOrCollection]) => {
			new MapParser().parseComplex(new CborObjectBuilder().mapKey[String], x)
		}
		case JVCSeq(x:Seq[JsonValueOrCollection]) => {
			new SeqParser().parseComplex(new CborArrayBuilder().mapKey[Int]{x => JsonValue(x)}, x)
		}
	}
	
	private[builder] def byteArray2Long(b:Seq[Byte]):Long = {
		b.foldLeft(0L){(x,y) => (x << 8) | (y.intValue & 0xFF)}
	}
	private[builder] def long2ByteArray(l:Long, count:Int = 8):Seq[Byte] = {
		(56 to 0 by -8).map{x => ((l >> x) & 0xFF).byteValue}.takeRight(count)
	}
}
