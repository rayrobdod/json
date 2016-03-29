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
package com.rayrobdod.json.parser;

import java.io.DataInput
import java.text.ParseException
import java.nio.charset.StandardCharsets.UTF_8;
import scala.collection.immutable.{Seq, Map, Stack}
import com.rayrobdod.json.builder._
import com.rayrobdod.json.union.JsonValue

/**
 * A parser that will decode cbor data
 * 
 * @see [[http://tools.ietf.org/html/rfc7049]]
 * 
 * @constructor
 * Creates a CborParser instance.
 */
// TODO: widen key to include everything else that a key can be (aka pretty much anything)
final class CborParser extends Parser[JsonValue, JsonValue, DataInput] {
	import CborParser._
	
	def parsePrimitive(i:DataInput):JsonValue = {
		val a = this.parse(new PrimitiveSeqBuilder, i)
		a match {
			case x:JsonValue => x
			case _ => throw new ParseException("Not a Primitive", -1)
		}
	}
	
	def parseComplex[A](builder:Builder[JsonValue, JsonValue, A], i:DataInput):A = {
		val a = this.parse(builder, i)
		a match {
			case x:A => x
			case _ => throw new ParseException("Was a Primitive", -1)
		}
	}
	
	
	/**
	 * Decodes the input values to an object.
	 */
	def parse[A](topBuilder:Builder[JsonValue, JsonValue, A], input:DataInput):Any = {
		val headerByte:Byte = input.readByte();
		val majorType = (headerByte >> 5) & 0x07
		val additionalInfo = headerByte & 0x1F
		val additionalInfoData:AdditionalInfoData = additionalInfo match {
			case x if (x <= 23) => { AdditionalInfoDeterminate(x) }
			case 24 => { AdditionalInfoDeterminate(input.readUnsignedByte()) }
			case 25 => { AdditionalInfoDeterminate(input.readUnsignedShort()) }
			case 26 => { AdditionalInfoDeterminate(input.readInt()) } //todo unsigned int
			case 27 => { AdditionalInfoDeterminate(input.readLong()) } // todo unsigned long (?)
			case 31 => { AdditionalInfoIndeterminate() }
			case _  => {throw new ParseException("Illegal `additionalInfo` field", -1)}
		}
		
		majorType match {
			// positive integer
			case MajorTypeCodes.POSITIVE_INT => JsonValue( additionalInfoData.value )
			// negative integer
			case MajorTypeCodes.NEGATIVE_INT => JsonValue( -1 - additionalInfoData.value )
			// byte string
			case MajorTypeCodes.BYTE_ARRAY => JsonValue( parseByteString(input, additionalInfoData) )
			// text string
			case MajorTypeCodes.STRING => JsonValue( new String(parseByteString(input, additionalInfoData), UTF_8) )
			// array/list
			case MajorTypeCodes.ARRAY => parseArray(topBuilder, input, additionalInfoData)
			// map
			case MajorTypeCodes.OBJECT => parseObject(topBuilder, input, additionalInfoData)
			// tags
			case MajorTypeCodes.TAG => {
				new TaggedValue(additionalInfoData.value, this.parse(topBuilder, input))
			}
			// floats/simple
			case MajorTypeCodes.SPECIAL => additionalInfo match {
				case SimpleValueCodes.FALSE => JsonValue( false )
				case SimpleValueCodes.TRUE  => JsonValue( true )
				case SimpleValueCodes.NULL => JsonValue.JsonValueNull
				case SimpleValueCodes.HALF_FLOAT => throw new UnsupportedOperationException("Half float")
				case SimpleValueCodes.FLOAT => JsonValue( java.lang.Float.intBitsToFloat(additionalInfoData.value.intValue))
				case SimpleValueCodes.DOUBLE => JsonValue( java.lang.Double.longBitsToDouble(additionalInfoData.value.longValue))
				case SimpleValueCodes.END_OF_LIST => EndOfIndeterminateObject()
				case _  => UnknownSimpleValue(additionalInfoData.value.byteValue)
			}
			case _ => throw new AssertionError("majorType was greater than 7")
		}
	}
	
	private[this] def parseByteString(input:DataInput, aid:AdditionalInfoData):Array[Byte] = {
		aid match {
			case AdditionalInfoIndeterminate() => {
				val stream = new java.io.ByteArrayOutputStream
				
				var next:Any = this.parse(new PrimitiveSeqBuilder, input)
				while (next != EndOfIndeterminateObject()) {
					val nextBytes:Array[Byte] = next match {
						case JsonValue.JsonValueString(s:String) => s.getBytes(UTF_8)
						case JsonValue.JsonValueByteStr(a:Array[Byte]) => a
						case _ => throw new ClassCastException("Members of indeterminite-length string must be strings")
					}
					stream.write(nextBytes)
					next = this.parse(new PrimitiveSeqBuilder, input)
				}
				stream.toByteArray()
			}
			case AdditionalInfoDeterminate(len:Long) => {
				val bytes = new Array[Byte](len.intValue)
				input.readFully(bytes)
				bytes
			}
		}
	}
	
	private[this] def parseArray[A](topBuilder:Builder[JsonValue, JsonValue, A], input:DataInput, aid:AdditionalInfoData):A = {
		var retVal:A = topBuilder.init
		
		aid match {
			case AdditionalInfoDeterminate(len:Long) => {
				(0 until len.intValue).foreach{index =>
					val childBuilder = topBuilder.apply[DataInput](JsonValue(index))
					retVal = childBuilder.apply(retVal, input, this)
				}
			}
			case AdditionalInfoIndeterminate() => {
				var index:Int = 0
				var childObject:Any = ""
				while (childObject != EndOfIndeterminateObject()) {
					val childBuilder = topBuilder.apply[JsonValue](JsonValue(index))
					
					childObject = this.parse(new SeqBuilder(new MapBuilder()), input)
					
					if (childObject != EndOfIndeterminateObject()) {
						retVal = childBuilder.apply(retVal, childObject.asInstanceOf[JsonValue], new IdentityParser())
						index = index + 1
					}
				}
			}
		}
		retVal
	}
	
	private[this] def parseObject[A](topBuilder:Builder[JsonValue, JsonValue, A], input:DataInput, aid:AdditionalInfoData):A = {
		var retVal:A = topBuilder.init
		
		aid match {
			case AdditionalInfoDeterminate(len:Long) => {
				(0 until len.intValue).foreach{index =>
					val keyObject = this.parsePrimitive(input)
					val childBuilder = topBuilder.apply[DataInput](keyObject)
					retVal = childBuilder.apply(retVal, input, this)
				}
			}
			case AdditionalInfoIndeterminate() => {
				var keyObject:Any = ""
				while (keyObject != EndOfIndeterminateObject()) {
					keyObject = this.parse(new PrimitiveSeqBuilder, input)
					if (keyObject != EndOfIndeterminateObject()) {
						val childBuilder = topBuilder.apply[DataInput](keyObject.asInstanceOf[JsonValue])
						retVal = childBuilder.apply(retVal, input, this)
					}
				}
			}
		}
		retVal
	}
}

/**
 * Objects related to Cbor's data model
 */
object CborParser {
	
	private abstract sealed class AdditionalInfoData {
		def value:Long
	}
	private final case class AdditionalInfoDeterminate(override val value:Long) extends AdditionalInfoData {
		
	}
	private final case class AdditionalInfoIndeterminate() extends AdditionalInfoData {
		override def value:Nothing = throw new UnsupportedOperationException
	}
	
	/**
	 * The marker of the end of an indeterminate value. Represented as (0xFF).
	 * Unless you're trying to see this value, you shouldn't see this value.
	 */
	final case class EndOfIndeterminateObject()
	/** A simple value other than the known ones */
	final case class UnknownSimpleValue(value:Byte)
	/** A tagged value */
	final case class TaggedValue(tag:Long, item:Any)
	
	/**
	 * The CBOR major types.
	 * Because magic numbers are bad.
	 */
	object MajorTypeCodes {
		val POSITIVE_INT:Byte = 0
		val NEGATIVE_INT:Byte = 1
		val BYTE_ARRAY:Byte = 2
		val STRING:Byte = 3
		val ARRAY:Byte = 4
		val OBJECT:Byte = 5
		val TAG:Byte = 6
		val SPECIAL:Byte = 7
	}
	
	/**
	 * Known simple values.
	 * Because magic numbers are bad.
	 */
	object SimpleValueCodes {
		val FALSE:Byte  = 20
		val TRUE:Byte   = 21
		val NULL:Byte   = 22
		val HALF_FLOAT:Byte = 25
		val FLOAT:Byte  = 26
		val DOUBLE:Byte = 27
		val END_OF_LIST:Byte = 31
	}
	
}
