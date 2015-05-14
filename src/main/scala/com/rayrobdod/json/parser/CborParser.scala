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

/**
 * A parser that will decode cbor data
 * 
 * == Primitive types ==
 * 
 - null
 - java.lang.Long
 - Array[Byte]
 - java.lang.String
 - java.lang.Boolean
 - java.lang.Float
 - java.lang.Double
 * 
 * @see [[http://tools.ietf.org/html/rfc7049]]
 * 
 * @constructor
 * Creates a CborParser instance.
 * @param topBuilder the builder that this parser will use when constructing objects
 */
final class CborParser[A](topBuilder:Builder[A]) {
	import CborParser._
	
	/**
	 * Decodes the input values to an object.
	 */
	def parse(input:DataInput):Any = {
		val headerByte:Byte = input.readByte();
		val majorType = (headerByte >> 5) & 0x07
		val additionalInfo = headerByte & 0x1F
		val additionalInfoData:AdditionalInfoData = {
			if (additionalInfo <= 23) { AdditionalInfoDeterminate(additionalInfo) }
			else if (additionalInfo == 24) { AdditionalInfoDeterminate(input.readUnsignedByte()) }
			else if (additionalInfo == 25) { AdditionalInfoDeterminate(input.readUnsignedShort()) }
			else if (additionalInfo == 26) { AdditionalInfoDeterminate(input.readInt()) } //todo unsigned int
			else if (additionalInfo == 27) { AdditionalInfoDeterminate(input.readLong()) } // todo unsigned long (?)
			else if (additionalInfo == 31) { AdditionalInfoIndeterminate() }
			else {throw new ParseException("Illegal `additionalInfo` field", -1)}
		}
		
		majorType match {
			// positive integer
			case 0 => additionalInfoData.value
			// negative integer
			case 1 => -1 - additionalInfoData.value
			// byte string
			case 2 => parseByteString(input, additionalInfoData)
			// text string
			case 3 => new String(parseByteString(input, additionalInfoData), UTF_8)
			// array/list
			case 4 => parseArray(topBuilder, input, additionalInfoData)
			// map
			case 5 => parseObject(topBuilder, input, additionalInfoData)
			// tags
			case 6 => {
				new TaggedValue(additionalInfoData.value, this.parse(input))
			}
			// floats/simple
			case 7 => additionalInfo match {
				case 20 => false
				case 21 => true
				case 22 => null
				case 25 => throw new UnsupportedOperationException("Half float")
				case 26 => java.lang.Float.intBitsToFloat(additionalInfoData.value.intValue)
				case 27 => java.lang.Double.longBitsToDouble(additionalInfoData.value.longValue)
				case 31 => EndOfIndeterminateObject()
				case _  => UnknownSimpleValue(additionalInfoData.value.byteValue)
			}
			case _ => throw new AssertionError("majorType was greater than 7")
		}
	}
	
	private def parseByteString(input:DataInput, aid:AdditionalInfoData):Array[Byte] = {
		aid match {
			case AdditionalInfoIndeterminate() => {
				val stream = new java.io.ByteArrayOutputStream
				
				var next:Any = this.parse(input)
				while (next != EndOfIndeterminateObject()) {
					val nextBytes:Array[Byte] = next match {
						case s:String => s.getBytes(UTF_8)
						case a:Array[Byte] => a
						case _ => throw new ClassCastException("Members of indeterminite-length string must be strings")
					}
					stream.write(nextBytes)
					next = this.parse(input)
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
	
	private def parseArray(topBuilder:Builder[A], input:DataInput, aid:AdditionalInfoData):A = {
		var retVal:A = topBuilder.init
		
		aid match {
			case AdditionalInfoDeterminate(len:Long) => {
				(0 until len.intValue).foreach{index =>
					val childParser = new CborParser(topBuilder.childBuilder(index.toString))
					val childObject = childParser.parse(input)
					retVal = topBuilder.apply(retVal, index.toString, childObject)
				}
			}
			case AdditionalInfoIndeterminate() => {
				var index:Int = 0
				var childObject:Any = ""
				while (childObject != EndOfIndeterminateObject()) {
					val childParser = new CborParser(topBuilder.childBuilder(index.toString))
					childObject = childParser.parse(input)
					
					if (childObject != EndOfIndeterminateObject()) {
						retVal = topBuilder.apply(retVal, index.toString, childObject)
						index = index + 1
					}
				}
			}
		}
		retVal
	}
	
	private def parseObject(topBuilder:Builder[A], input:DataInput, aid:AdditionalInfoData):A = {
		var retVal:A = topBuilder.init
		
		aid match {
			case AdditionalInfoDeterminate(len:Long) => {
				(0 until len.intValue).foreach{index =>
					val keyParser = new CborParser(topBuilder) // in other words, pray that the key is not an object or array
					val keyObject = keyParser.parse(input)
					val childParser = new CborParser(topBuilder.childBuilder(index.toString))
					val childObject = childParser.parse(input)
					
					retVal = topBuilder.apply(retVal, keyObject.toString, childObject)
				}
			}
			case AdditionalInfoIndeterminate() => {
				var keyObject:Any = ""
				while (keyObject != EndOfIndeterminateObject()) {
					val keyParser = new CborParser(topBuilder) // in other words, pray that the key is not an object or array
					keyObject = keyParser.parse(input)
					if (keyObject != EndOfIndeterminateObject()) {
						val childParser = new CborParser(topBuilder.childBuilder(keyObject.toString))
						val childObject = childParser.parse(input)
						
						retVal = topBuilder.apply(retVal, keyObject.toString, childObject)
					}
				}
			}
		}
		retVal
	}
}


private object CborParser {
	private abstract sealed class AdditionalInfoData {
		def value:Long
	}
	private final case class AdditionalInfoDeterminate(override val value:Long) extends AdditionalInfoData {
		
	}
	private final case class AdditionalInfoIndeterminate() extends AdditionalInfoData {
		override def value:Nothing = throw new UnsupportedOperationException
	}
	
	final case class EndOfIndeterminateObject()
	final case class UnknownSimpleValue(value:Byte)
	final case class TaggedValue(tag:Long, item:Any)
}
