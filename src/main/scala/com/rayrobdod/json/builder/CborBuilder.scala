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
import com.rayrobdod.json.union.{CborValue, ParserRetVal}
import com.rayrobdod.json.union.ParserRetVal.{Complex, BuilderFailure}
import com.rayrobdod.json.parser.{Parser, CborParser, byteArray2DataInput}

/**
 * A builder whose output is a cbor-formatted byte string.
 * 
 * @since 3.0
 * @version 4.0
 * @see [[http://tools.ietf.org/html/rfc7049]]
 * @constructor
 * A builder that will create cbor object format byte strings
 * @param forceObject true if the builder should create an object even if it is possible to create an array from the inputs
 */
final class CborBuilder(forceObject:Boolean = false) extends Builder[CborValue, CborValue, IllegalFoldingFailure.type, Seq[Byte]] {
	import CborBuilder._
	
	override type Middle = CborBuilder.Middle
	
	override val init:Middle = new Middle(forceObject)
	
	override def apply[Input, PF, BE](folding:Middle, key:CborValue, input:Input, parser:Parser[CborValue, CborValue, PF, BE, Input], extra:BE):ParserRetVal[Middle, Nothing, PF, IllegalFoldingFailure.type, BE] = {
		val value = parser.parse[Seq[Byte], IllegalFoldingFailure.type](this, input)
		val encodedValueOpt = value.primitive.map{encodeValue}.mergeToComplex
		encodedValueOpt.complex.flatMap{encodedValue =>
			
			val foldingIsObject = folding.isObject
			val keyRequiresObject = key match {
				case CborValue.CborValueNumber(x) => x != CborValue.Rational(folding.count)
				case _ => true
			}
			val resultIsObject = foldingIsObject || keyRequiresObject
			
			
			if (resultIsObject) {
				Complex( folding.convertToObject.append( encodeValue(key) ++ encodedValue ) )
			} else {
				Complex( folding.append(encodedValue) )
			}
		}
	}
	
	override def finish[BE](extra:BE)(folding:Middle):ParserRetVal.Complex[Seq[Byte]] = ParserRetVal.Complex(folding.finish())
}

object CborBuilder {
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
	
	private[this] def long2ByteArray(l:Long, count:Int = 8):Seq[Byte] = {
		(56 to 0 by -8).map{x => ((l >> x) & 0xFF).byteValue}.takeRight(count)
	}
	
	/** CborBuilder's Middle type */
	final case class Middle private[builder] (
		  val isObject:Boolean
		, val count:Long
		, private val parts:List[Seq[Byte]]
	) {
		private[builder] def this(forceObject:Boolean = false) = this(forceObject, 0, Nil)
		
		private[CborBuilder] def convertToObject:Middle = {
			if (this.isObject) {
				this
			} else {
				new Middle(
					  true
					, this.count
					, this.parts.reverse.zipWithIndex.map{case (value, key) => encodeValue(key) ++ value}.reverse
				)
			}
		}
		
		private[CborBuilder] def append(x:Seq[Byte]):Middle = new Middle(
			  this.isObject
			, this.count + 1
			, x :: this.parts
		)
		
		private[CborBuilder] def finish():Seq[Byte] = {
			val typeCode = if (this.isObject) {MajorTypeCodes.OBJECT} else {MajorTypeCodes.ARRAY}
			encodeLength(typeCode, this.count) ++ this.parts.reverse.flatten
		}
	}
}
