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
package com.rayrobdod.json.parser;

import java.io.DataInput
import java.text.ParseException
import java.nio.charset.StandardCharsets.UTF_8;
import scala.collection.immutable.{Seq, Map, Stack}
import scala.util.{Try, Success, Failure}
import com.rayrobdod.json.builder._
import com.rayrobdod.json.union.JsonValue

/**
 * A parser that will decode cbor data.
 * 
 * This cannot handle complex values in map keys.
 * 
 * @version next
 * @see [[http://tools.ietf.org/html/rfc7049]]
 * 
 * @constructor
 * Creates a CborParser instance.
 */
final class CborParser extends Parser[JsonValue, JsonValue, DataInput] {
	import CborParser._
	
	override def parse[ComplexOutput](builder:Builder[JsonValue, JsonValue, ComplexOutput], i:DataInput):Try[Either[ComplexOutput, JsonValue]] = {
		val a = this.parseDetailed[ComplexOutput](builder, i)
		a match {
			case ParseReturnValueSimple(x:JsonValue) => Try(Right(x))
			case ParseReturnValueComplex(x) => Try(Left(x))
			case ParseReturnValueFailure(x) => Failure(x)
			case _ => Failure(new ParseException("Not a public value", 0))
		}
	}
	
	
	/**
	 * Decodes the input values to an object.
	 */
	def parseDetailed[A](topBuilder:Builder[JsonValue, JsonValue, A], input:DataInput):ParseReturnValue[A] = {
		val headerByte:Byte = input.readByte();
		val majorType = (headerByte >> 5) & 0x07
		val additionalInfo = headerByte & 0x1F
		val additionalInfoDataTry = additionalInfo match {
			case x if (x <= 23) => { Right(AdditionalInfoDeterminate(x)) }
			case 24 => { Right(AdditionalInfoDeterminate(input.readUnsignedByte())) }
			case 25 => { Right(AdditionalInfoDeterminate(input.readUnsignedShort())) }
			case 26 => { Right(AdditionalInfoDeterminate(input.readInt())) } //todo unsigned int
			case 27 => { Right(AdditionalInfoDeterminate(input.readLong())) } // todo unsigned long (?)
			case 31 => { Right(AdditionalInfoIndeterminate()) }
			case _  => { Left(new ParseException("Illegal `additionalInfo` field", -1)) }
		}
		
		additionalInfoDataTry.fold({x =>
			new ParseReturnValueFailure(x)
		}, {additionalInfoData =>
			majorType match {
				// positive integer
				case MajorTypeCodes.POSITIVE_INT => additionalInfoData match {
					case AdditionalInfoDeterminate(value) => ParseReturnValueSimple(JsonValue( value ))
					case x:AdditionalInfoIndeterminate => ParseReturnValueFailure(new UnsupportedOperationException("Indeterminate integer value"))
				}
				// negative integer
				case MajorTypeCodes.NEGATIVE_INT => additionalInfoData match {
					case AdditionalInfoDeterminate(value) => ParseReturnValueSimple(JsonValue( -1 - value ))
					case x:AdditionalInfoIndeterminate => ParseReturnValueFailure(new UnsupportedOperationException("Indeterminate integer value"))
				}
				// byte string
				case MajorTypeCodes.BYTE_ARRAY => parseByteString(input, additionalInfoData) match {
					case Success(x) => ParseReturnValueSimple(JsonValue(x))
					case Failure(x) => ParseReturnValueFailure(x)
				}
				// text string
				case MajorTypeCodes.STRING => parseByteString(input, additionalInfoData) match {
					case Success(x) => ParseReturnValueSimple(JsonValue(new String(x, UTF_8)))
					case Failure(x) => ParseReturnValueFailure(x)
				}
				// array/list
				case MajorTypeCodes.ARRAY => parseArray(topBuilder, input, additionalInfoData) match {
					case Success(x) => ParseReturnValueComplex(x)
					case Failure(x) => ParseReturnValueFailure(x)
				}
				// map
				case MajorTypeCodes.OBJECT => parseObject(topBuilder, input, additionalInfoData) match {
					case Success(x) => ParseReturnValueComplex(x)
					case Failure(x) => ParseReturnValueFailure(x)
				}
				// tags
				case MajorTypeCodes.TAG => additionalInfoData match {
					case AdditionalInfoDeterminate(value) => new ParseReturnValueTaggedValue(value, this.parseDetailed(topBuilder, input))
					case x:AdditionalInfoIndeterminate => ParseReturnValueFailure(new UnsupportedOperationException("Indeterminate tag value"))
				}
				// floats/simple
				case MajorTypeCodes.SPECIAL => additionalInfo match {
					case SimpleValueCodes.FALSE => ParseReturnValueSimple(JsonValue( false ))
					case SimpleValueCodes.TRUE  => ParseReturnValueSimple(JsonValue( true ))
					case SimpleValueCodes.NULL => ParseReturnValueSimple(JsonValue.JsonValueNull)
					case SimpleValueCodes.HALF_FLOAT => ParseReturnValueFailure(new UnsupportedOperationException("Half float"))
					case SimpleValueCodes.FLOAT => additionalInfoData match {
						case AdditionalInfoDeterminate(value) => ParseReturnValueSimple(JsonValue( java.lang.Float.intBitsToFloat(value.intValue)))
						case x:AdditionalInfoIndeterminate => ParseReturnValueFailure(new UnsupportedOperationException("Indeterminate tag value"))
					}
					case SimpleValueCodes.DOUBLE => additionalInfoData match {
						case AdditionalInfoDeterminate(value) => ParseReturnValueSimple(JsonValue( java.lang.Double.longBitsToDouble(value.longValue)))
						case x:AdditionalInfoIndeterminate => ParseReturnValueFailure(new UnsupportedOperationException("Indeterminate tag value"))
					}
					case SimpleValueCodes.END_OF_LIST => new ParseReturnValueEndOfIndeterminateObject
					case _  => additionalInfoData match {
						case AdditionalInfoDeterminate(value) => ParseReturnValueUnknownSimple(value.byteValue)
						case x:AdditionalInfoIndeterminate => ParseReturnValueFailure(new UnsupportedOperationException("Indeterminate tag value"))
					}
				}
				case _ => ParseReturnValueFailure(new AssertionError("majorType was greater than 7"))
			}
		})
	}
	
	private[this] def parseByteString(input:DataInput, aid:AdditionalInfoData):Try[Array[Byte]] = {
		aid match {
			case AdditionalInfoIndeterminate() => Try{
				val stream = new java.io.ByteArrayOutputStream
				
				var next:Any = this.parseDetailed(new PrimitiveSeqBuilder, input)
				while (next != ParseReturnValueEndOfIndeterminateObject()) {
					val nextBytes:Array[Byte] = next match {
						case ParseReturnValueSimple(JsonValue.JsonValueString(s:String)) => s.getBytes(UTF_8)
						case ParseReturnValueSimple(JsonValue.JsonValueByteStr(a:Array[Byte])) => a
						case _ => throw new ClassCastException("Members of indeterminite-length string must be strings")
					}
					stream.write(nextBytes)
					next = this.parseDetailed(new PrimitiveSeqBuilder, input)
				}
				stream.toByteArray()
			}
			case AdditionalInfoDeterminate(len:Long) => Try{
				val bytes = new Array[Byte](len.intValue)
				input.readFully(bytes)
				bytes
			}
		}
	}
	
	private[this] def parseArray[A](topBuilder:Builder[JsonValue, JsonValue, A], input:DataInput, aid:AdditionalInfoData):Try[A] = {
		var retVal:Try[A] = Try(topBuilder.init)
		
		aid match {
			case AdditionalInfoDeterminate(len:Long) => {
				(0 until len.intValue).foreach{index =>
					retVal = retVal.flatMap{x => topBuilder.apply[DataInput](x, JsonValue(index), input, this)}
				}
			}
			case AdditionalInfoIndeterminate() => {
				var index:Int = 0
				var childObject:ParseReturnValue[Seq[Byte]] = ParseReturnValueUnknownSimple(0)
				while (childObject != ParseReturnValueEndOfIndeterminateObject()) {
					childObject = this.parseDetailed(new CborBuilder(true), input)
					
					childObject match {
						case ParseReturnValueEndOfIndeterminateObject() => {}
						case ParseReturnValueSimple(x) => {
							retVal = retVal.flatMap{y => topBuilder.apply[JsonValue](y, JsonValue(index), x, new IdentityParser())}
						}
						case ParseReturnValueComplex(x) => {
							retVal = retVal.flatMap{y => topBuilder.apply[DataInput](y, JsonValue(index), byteArray2DataInput(x.toArray), this)}
						}
						case _ => retVal = Failure( new UnsupportedOperationException("Value not public"))
					}
					index = index + 1
				}
			}
		}
		retVal
	}
	
	private[this] def parseObject[A](topBuilder:Builder[JsonValue, JsonValue, A], input:DataInput, aid:AdditionalInfoData):Try[A] = {
		var retVal:Try[A] = Try(topBuilder.init)
		
		aid match {
			case AdditionalInfoDeterminate(len:Long) => {
				(0 until len.intValue).foreach{index =>
					val keyTry:Try[JsonValue] = this.parseDetailed(new ThrowBuilder, input) match {
						case ParseReturnValueSimple(x) => Success(x)
						case ParseReturnValueFailure(x) => Failure(x)
						case _ => Failure(new UnsupportedOperationException("Cannot handle non-simple map keys"))
					}
					retVal = {for ( foldingObject <- retVal; keyObject <- keyTry ) yield {
						topBuilder.apply[DataInput](foldingObject, keyObject, input, this)
					}}.flatten
				}
			}
			case AdditionalInfoIndeterminate() => {
				var keyObject:ParseReturnValue[_] = ParseReturnValueUnknownSimple(0)
				while (keyObject != ParseReturnValueEndOfIndeterminateObject()) {
					keyObject = this.parseDetailed(new ThrowBuilder, input)
					keyObject match {
						case ParseReturnValueEndOfIndeterminateObject() => {}
						case ParseReturnValueSimple(x) => {
							retVal = retVal.flatMap{y => topBuilder.apply[DataInput](y, x, input, this)}
						}
						case ParseReturnValueFailure(x) => retVal = Failure(x)
						case _ => retVal = Failure( new UnsupportedOperationException("Cannot handle non-simple map keys"))
					}
				}
			}
		}
		retVal
	}
}

/**
 * Objects related to Cbor's data model
 * @version next
 */
object CborParser {
	
	/** The 'length' value provided in an item's header */
	private sealed trait AdditionalInfoData
	private final case class AdditionalInfoDeterminate(val value:Long) extends AdditionalInfoData
	private final case class AdditionalInfoIndeterminate() extends AdditionalInfoData
	
	/** Possible return values of [[CborParser#parseDetailed]] */
	sealed trait ParseReturnValue[+A]
	final case class ParseReturnValueSimple(x:JsonValue) extends ParseReturnValue[Nothing]
	final case class ParseReturnValueComplex[A](a:A) extends ParseReturnValue[A]
	/**
	 * The marker of the end of an indeterminate value. Represented as (0xFF).
	 * Unless you're trying to see this value, you shouldn't see this value.
	 */
	final case class ParseReturnValueEndOfIndeterminateObject() extends ParseReturnValue[Nothing]
	/** A tagged value */
	final case class ParseReturnValueTaggedValue[A](tag:Long, x:ParseReturnValue[A]) extends ParseReturnValue[A]
	/** A simple value other than the known ones */
	final case class ParseReturnValueUnknownSimple(value:Byte) extends ParseReturnValue[Nothing]
	final case class ParseReturnValueFailure(ex:Throwable) extends ParseReturnValue[Nothing]
	
	
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
