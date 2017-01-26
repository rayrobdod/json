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
package com.rayrobdod.json.builder;

import scala.collection.immutable.Seq
import java.nio.charset.StandardCharsets.UTF_8
import com.rayrobdod.json.parser.CborParser.{MajorTypeCodes, SimpleValueCodes, TagCodes}
import com.rayrobdod.json.union.CborValue
import com.rayrobdod.json.parser.{Parser, CborParser, byteArray2DataInput}

/**
 * A builder whose output is a cbor-formatted byte string.
 * 
 * @since 3.0
 * @see [[http://tools.ietf.org/html/rfc7049]]
 * @constructor
 * A builder that will create cbor object format byte strings
 * @param forceObject true if the builder should create an object even if it is possible to create an array from the inputs
 */
final class CborBuilder(forceObject:Boolean = false) extends Builder[CborValue, CborValue, Seq[Byte]] {
	import CborBuilder._
	
	/** The bytes to encode a zero-length array or object  */
	override val init:Seq[Byte] = encodeLength((if (forceObject) {MajorTypeCodes.OBJECT} else {MajorTypeCodes.ARRAY}), 0)
	
	override def apply[Input](folding:Seq[Byte], key:CborValue, input:Input, parser:Parser[CborValue, CborValue, Input]):Either[(String, Int), Seq[Byte]] = {
		val value = parser.parse[Seq[Byte]](this, input)
		val encodedValueOpt = value.primitive.map{encodeValue}.mergeToEither
		encodedValueOpt.right.flatMap{encodedValue =>
		
			val headerByte:Byte = folding.head
			val majorType = (headerByte >> 5) & 0x07
			val additionalInfo = headerByte & 0x1F
			val (objectLength:Long, passData:Seq[Byte]) = additionalInfo match {
				case x if (x <= 23) => { (x.longValue, folding.tail) }
				case 24 => { (folding(1).longValue & 0xFF, folding.tail.tail) }
				case 25 => { (byteArray2Long(folding.drop(1).take(2)), folding.drop(3)) }
				case 26 => { (byteArray2Long(folding.drop(1).take(4)), folding.drop(5)) }
				case 27 => { (byteArray2Long(folding.drop(1).take(8)), folding.drop(9)) }
				case _  => {throw new IllegalArgumentException("input `folding` had illegal length value")}
			}
			
			if (majorType == MajorTypeCodes.ARRAY) {
				key match {
					case CborValue.CborValueNumber(r) if (r.tryToLong == Option(objectLength)) => {
						// continue being array
						Right(encodeLength(MajorTypeCodes.ARRAY, objectLength + 1) ++ passData ++ encodedValue)
					}
					case _ => {
						// convert into object
						val newBuilder = new CborBuilder(true)
						val convertedBytesTry = new CborParser().parse(newBuilder, byteArray2DataInput(folding.toArray)).fold({s => Right(s)}, {p => Left("invalid folding param: " + java.util.Arrays.toString(folding.toArray), 0)}, {(s,i) => Left((s,i))})
						convertedBytesTry.right.map{convertedBytes =>
							encodeLength(MajorTypeCodes.OBJECT, objectLength + 1) ++ convertedBytes.drop(folding.length - passData.length) ++ encodeValue(key) ++ encodedValue
						}
					}
				}
			} else if (majorType == MajorTypeCodes.OBJECT) {
				
				Right(encodeLength(MajorTypeCodes.OBJECT, objectLength + 1) ++ passData ++ encodeValue(key) ++ encodedValue)
			} else {
				
				Left("Invalid folding parameter", 0)
			}
		}
	}
}

private object CborBuilder {
	import com.rayrobdod.json.union.CborValue._
	
	/**
	 * Encode a number into CBOR form.
	 * @param majorType the major type to prepend to the number. 0 ≥ x ≥ 7
	 * @param value the value to encode
	 */
	private def encodeLength(majorType:Byte, value:Long):Seq[Byte] = {
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
	
	private def encodeValue(v:CborValue):Seq[Byte] = v match {
		case CborValueBoolean(false) => encodeLength(MajorTypeCodes.SPECIAL, SimpleValueCodes.FALSE)
		case CborValueBoolean(true)  => encodeLength(MajorTypeCodes.SPECIAL, SimpleValueCodes.TRUE)
		case CborValueNull  => encodeLength(MajorTypeCodes.SPECIAL, SimpleValueCodes.NULL)
		case CborValueNumber(value) => {
			value.tryToBigInt.fold{
				value.tryToFloat.fold{
					value.tryToDouble.fold{
						value.tryToBigDecimal.fold{
							val r = value.reduce
							encodeLength(MajorTypeCodes.TAG, TagCodes.RATIONAL) ++
									encodeLength(MajorTypeCodes.ARRAY, 2) ++
									encodeValue(CborValueNumber(r.num)) ++
									encodeValue(CborValueNumber(r.denom))
						}{d:BigDecimal =>
							val d2 = d.underlying.stripTrailingZeros
							encodeLength(MajorTypeCodes.TAG, TagCodes.BIG_DECIMAL) ++
									encodeLength(MajorTypeCodes.ARRAY, 2) ++
									encodeValue(CborValueNumber(- d2.scale)) ++
									encodeValue(CborValueNumber(d2.unscaledValue))
							
						}
					}{d:Double =>
						Seq((0xE0 + SimpleValueCodes.DOUBLE).byteValue) ++ long2ByteArray(java.lang.Double.doubleToLongBits(d))
					}
				}{f:Float =>
					Seq((0xE0 + SimpleValueCodes.FLOAT).byteValue) ++ long2ByteArray(java.lang.Float.floatToIntBits(f), 4)
				}
			}{i:BigInt =>
				if (i.isValidLong) {
					if (i >= 0) {
						encodeLength(MajorTypeCodes.POSITIVE_INT, i.longValue)
					} else {
						encodeLength(MajorTypeCodes.NEGATIVE_INT, -1 - i.longValue)
					}
				} else {
					if (i >= 0) {
						encodeLength(MajorTypeCodes.TAG, TagCodes.POS_BIG_INT) ++
								encodeValue(CborValueByteStr(i.toByteArray))
					} else {
						encodeLength(MajorTypeCodes.TAG, TagCodes.NEG_BIG_INT) ++
								encodeValue(CborValueByteStr((-1 - i).toByteArray))
					}
				}
			}
			
		}
		case CborValueString(x:String) => {
			val bytes = x.getBytes(UTF_8)
			encodeLength(MajorTypeCodes.STRING, bytes.length) ++ bytes
		}
		case CborValueByteStr(bytes:Array[Byte]) => encodeLength(MajorTypeCodes.BYTE_ARRAY, bytes.length) ++ bytes
	}
	
	private[builder] def byteArray2Long(b:Seq[Byte]):Long = {
		b.foldLeft(0L){(x,y) => (x << 8) | (y.intValue & 0xFF)}
	}
	private[builder] def long2ByteArray(l:Long, count:Int = 8):Seq[Byte] = {
		(56 to 0 by -8).map{x => ((l >> x) & 0xFF).byteValue}.takeRight(count)
	}
}
