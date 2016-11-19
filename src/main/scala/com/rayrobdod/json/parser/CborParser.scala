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
import java.nio.charset.StandardCharsets.UTF_8;
import com.rayrobdod.json.builder.{Builder, PrimitiveSeqBuilder, CborBuilder, ThrowBuilder}
import com.rayrobdod.json.union.{CborValue, ParserRetVal, Numeric}

/**
 * A parser that will decode cbor data.
 * 
 * This does not support
   - complex values in map keys
   - halffloats
   - any tags
 * 
 * @version 3.0
 * @see [[http://tools.ietf.org/html/rfc7049]]
 * 
 * @constructor
 * Creates a CborParser instance.
 */
final class CborParser extends Parser[CborValue, CborValue, DataInput] {
	import CborParser._
	
	override def parse[ComplexOutput](builder:Builder[CborValue, CborValue, ComplexOutput], i:DataInput):ParserRetVal[ComplexOutput, CborValue] = {
		import ParserRetVal.{Primitive, Complex, Failure}
		val a = this.parseDetailed[ComplexOutput](builder, i)
		a match {
			case ParseReturnValueSimple(x:CborValue) => Primitive(x)
			case ParseReturnValueComplex(x) => Complex(x)
			case ParseReturnValueFailure(msg, idx) => Failure(msg, idx)
			case _ => Failure("Not a public value", 0)
		}
	}
	
	
	/**
	 * Decodes the input values to an object.
	 */
	def parseDetailed[A](topBuilder:Builder[CborValue, CborValue, A], input:DataInput):ParseReturnValue[A] = {
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
			case _  => { Left("Illegal `additionalInfo` field", 0) }
		}
		
		additionalInfoDataTry.fold({x =>
			new ParseReturnValueFailure(x._1, x._2)
		}, {additionalInfoData =>
			majorType match {
				// positive integer
				case MajorTypeCodes.POSITIVE_INT => additionalInfoData match {
					case AdditionalInfoDeterminate(value) => ParseReturnValueSimple(CborValue( value ))
					case x:AdditionalInfoIndeterminate => ParseReturnValueFailure("Indeterminate integer value", 0)
				}
				// negative integer
				case MajorTypeCodes.NEGATIVE_INT => additionalInfoData match {
					case AdditionalInfoDeterminate(value) => ParseReturnValueSimple(CborValue( -1 - value ))
					case x:AdditionalInfoIndeterminate => ParseReturnValueFailure("Indeterminate integer value", 0)
				}
				// byte string
				case MajorTypeCodes.BYTE_ARRAY => parseByteString(input, additionalInfoData) match {
					case Right(x) => ParseReturnValueSimple(CborValue(x))
					case Left(x) => ParseReturnValueFailure(x._1, x._2)
				}
				// text string
				case MajorTypeCodes.STRING => parseByteString(input, additionalInfoData) match {
					case Right(x) => ParseReturnValueSimple(CborValue(new String(x, UTF_8)))
					case Left(x) => ParseReturnValueFailure(x._1, x._2)
				}
				// array/list
				case MajorTypeCodes.ARRAY => parseArray(topBuilder, input, additionalInfoData) match {
					case Right(x) => ParseReturnValueComplex(x)
					case Left(x) => ParseReturnValueFailure(x._1, x._2)
				}
				// map
				case MajorTypeCodes.OBJECT => parseObject(topBuilder, input, additionalInfoData) match {
					case Right(x) => ParseReturnValueComplex(x)
					case Left(x) => ParseReturnValueFailure(x._1, x._2)
				}
				// tags
				case MajorTypeCodes.TAG => additionalInfoData match {
					case AdditionalInfoDeterminate(value) => new ParseReturnValueTaggedValue(value, this.parseDetailed(topBuilder, input))
					case x:AdditionalInfoIndeterminate => ParseReturnValueFailure("Indeterminate tag value", 0)
				}
				// floats/simple
				case MajorTypeCodes.SPECIAL => additionalInfo match {
					case SimpleValueCodes.FALSE => ParseReturnValueSimple(CborValue( false ))
					case SimpleValueCodes.TRUE  => ParseReturnValueSimple(CborValue( true ))
					case SimpleValueCodes.NULL => ParseReturnValueSimple(CborValue.CborValueNull)
					case SimpleValueCodes.HALF_FLOAT => additionalInfoData match {
						case AdditionalInfoDeterminate(value) => ParseReturnValueSimple(CborValue(value.shortValue)(Numeric.HalfFloatNumeric))
						case x:AdditionalInfoIndeterminate => ParseReturnValueFailure("Indeterminate special value", 0)
					}
					case SimpleValueCodes.FLOAT => additionalInfoData match {
						case AdditionalInfoDeterminate(value) => ParseReturnValueSimple(CborValue( java.lang.Float.intBitsToFloat(value.intValue)))
						case x:AdditionalInfoIndeterminate => ParseReturnValueFailure("Indeterminate special value", 0)
					}
					case SimpleValueCodes.DOUBLE => additionalInfoData match {
						case AdditionalInfoDeterminate(value) => ParseReturnValueSimple(CborValue( java.lang.Double.longBitsToDouble(value.longValue)))
						case x:AdditionalInfoIndeterminate => ParseReturnValueFailure("Indeterminate special value", 0)
					}
					case SimpleValueCodes.END_OF_LIST => new ParseReturnValueEndOfIndeterminateObject
					case _  => additionalInfoData match {
						case AdditionalInfoDeterminate(value) => ParseReturnValueUnknownSimple(value.byteValue)
						case x:AdditionalInfoIndeterminate => ParseReturnValueFailure("Indeterminate special value", 0)
					}
				}
				// `whatver & 7` can only be a value between 0 through 7 inclusive, but
				// scala's type system does not know that, hence this unreachable statement.
				case _ => ParseReturnValueFailure("majorType was greater than 7", 0)
			}
		})
	}
	
	private[this] def parseByteString(input:DataInput, aid:AdditionalInfoData):Either[(String, Int), Array[Byte]] = {
		class WrongStringTypeException(msg:String) extends Exception(msg)
		
		try {
			aid match {
				case AdditionalInfoIndeterminate() => {
					val stream = new java.io.ByteArrayOutputStream
					
					var next:Any = this.parseDetailed(new PrimitiveSeqBuilder, input)
					while (next != ParseReturnValueEndOfIndeterminateObject()) {
						val nextBytes:Array[Byte] = next match {
							case ParseReturnValueSimple(CborValue.CborValueString(s:String)) => s.getBytes(UTF_8)
							case ParseReturnValueSimple(CborValue.CborValueByteStr(a:Array[Byte])) => a
							case _ => throw new WrongStringTypeException("Members of indeterminite-length string must be strings")
						}
						stream.write(nextBytes)
						next = this.parseDetailed(new PrimitiveSeqBuilder, input)
					}
					Right(stream.toByteArray())
				}
				case AdditionalInfoDeterminate(len:Long) => {
					val bytes = new Array[Byte](len.intValue)
					input.readFully(bytes)
					Right(bytes)
				}
			}
		} catch {
			case ex:java.io.IOException => Left(ex.getMessage(), 0)
			case ex:WrongStringTypeException => Left(ex.getMessage(), 0)
		}
	}
	
	private[this] def parseArray[A](topBuilder:Builder[CborValue, CborValue, A], input:DataInput, aid:AdditionalInfoData):Either[(String, Int), A] = {
		var retVal:Either[(String, Int), A] = Right(topBuilder.init)
		
		aid match {
			case AdditionalInfoDeterminate(len:Long) => {
				(0 until len.intValue).foreach{index =>
					retVal = retVal.right.flatMap{x => topBuilder.apply[DataInput](x, CborValue(index), input, this)}
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
							retVal = retVal.right.flatMap{y => topBuilder.apply[CborValue](y, CborValue(index), x, new IdentityParser())}
						}
						case ParseReturnValueComplex(x) => {
							retVal = retVal.right.flatMap{y => topBuilder.apply[DataInput](y, CborValue(index), byteArray2DataInput(x.toArray), this)}
						}
						case _ => retVal = Left("Value not public", 0)
					}
					index = index + 1
				}
			}
		}
		retVal
	}
	
	private[this] def parseObject[A](topBuilder:Builder[CborValue, CborValue, A], input:DataInput, aid:AdditionalInfoData):Either[(String, Int), A] = {
		var retVal:Either[(String, Int), A] = Right(topBuilder.init)
		
		aid match {
			case AdditionalInfoDeterminate(len:Long) => {
				(0 until len.intValue).foreach{index =>
					val keyTry:Either[(String, Int), CborValue] = this.parseDetailed(new ThrowBuilder, input) match {
						case ParseReturnValueSimple(x) => Right(x)
						case ParseReturnValueFailure(msg,idx) => Left((msg,idx))
						case _ => Left("Cannot handle non-simple map keys",0)
					}
					retVal = {for ( foldingObject <- retVal.right; keyObject <- keyTry.right ) yield {
						topBuilder.apply[DataInput](foldingObject, keyObject, input, this)
					}}.right.flatMap{x => x}
				}
			}
			case AdditionalInfoIndeterminate() => {
				var keyObject:ParseReturnValue[_] = ParseReturnValueUnknownSimple(0)
				while (keyObject != ParseReturnValueEndOfIndeterminateObject()) {
					keyObject = this.parseDetailed(new ThrowBuilder, input)
					keyObject match {
						case ParseReturnValueEndOfIndeterminateObject() => {}
						case ParseReturnValueSimple(x) => {
							retVal = retVal.right.flatMap{y => topBuilder.apply[DataInput](y, x, input, this)}
						}
						case ParseReturnValueFailure(msg,idx) => retVal = Left((msg,idx))
						case _ => retVal = Left("Cannot handle non-simple map keys",0)
					}
				}
			}
		}
		retVal
	}
}

/**
 * Objects related to Cbor's data model
 * @version 3.0
 */
object CborParser {
	
	/** The 'length' value provided in an item's header */
	private sealed trait AdditionalInfoData
	private final case class AdditionalInfoDeterminate(val value:Long) extends AdditionalInfoData
	private final case class AdditionalInfoIndeterminate() extends AdditionalInfoData
	
	/** Possible return values of [[CborParser#parseDetailed]] */
	sealed trait ParseReturnValue[+A]
	final case class ParseReturnValueSimple(x:CborValue) extends ParseReturnValue[Nothing]
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
	final case class ParseReturnValueFailure(msg:String, idx:Int) extends ParseReturnValue[Nothing]
	
	//val selfdescribeTag:PartialFunction[Int, Function1[DataInput, ???]] = {
	//	case 55799 => {i:DataInput => new CborParser().parse[A](i)}
	//}
	
	
	import CborValue._
	import scala.math.{BigDecimal, BigInt}
	import spire.math.Rational
	val numberTags:PartialFunction[Int,Function1[DataInput, Either[(String,Int),CborValue]]] = {
		case 2 /* Positive bignum */ => {i:DataInput =>
			val bsOpt = new CborParser().parsePrimitive(i)
			bsOpt.right.flatMap{_ match {
				case CborValueByteStr(bs)=> Right(CborValueNumber(bs.foldLeft(0:BigInt){(a,b) => (a * 0x100) + ((b:Int) & (0xFF))}, implicitly[Numeric[BigInt]]))
				case _ => Left("Tag 2 contained non-byte-string value", 0)
			}}
		}
		case 3 /* Negative bignum */ => {i:DataInput =>
			val bsOpt = new CborParser().parsePrimitive(i)
			bsOpt.right.flatMap{_ match {
				case CborValueByteStr(bs)=> Right(CborValueNumber((-1:BigInt) - bs.foldLeft(0:BigInt){(a,b) => (a * 0x100) + ((b:Int) & (0xFF))}, implicitly[Numeric[BigInt]]))
				case _ => Left("Tag 3 contained non-byte-string value", 0)
			}}
		}
		case 4 /* Decimal Bigfloat */ => {i:DataInput =>
			new CborParser().parse(new PairBigIntBuilder("Tag 4"), i).fold(
				{x => val (a,b) = x; Right(BigDecimal(a) * BigDecimal(10).pow(b.intValue))},
				{x => Left("Tag 4 contained primitive", 0)},
				{(s,i) => Left(s,i)}
			)
		}
		case 5 /* Binary Bigfloat */ => {i:DataInput =>
			new CborParser().parse(new PairBigIntBuilder("Tag 5"), i).fold(
				{x => val (a,b) = x; Right(BigDecimal(a) * BigDecimal(2).pow(b.intValue))},
				{x => Left("Tag 5 contained primitive", 0)},
				{(s,i) => Left(s,i)}
			)
		}
		case 30 /* rational */ => {i:DataInput =>
			new CborParser().parse(new PairBigIntBuilder("Tag 30"), i).fold(
				{x => val (a,b) = x; Right(Rational(a,b))},
				{x => Left("Tag 30 contained primitive", 0)},
				{(s,i) => Left(s,i)}
			)
		}
	}
	
	private[this] class PairBigIntBuilder(tagNumber:String) extends Builder[CborValue, CborValue, (BigInt, BigInt)] {
		private[this] def nonIntError:String = tagNumber + " array contained a non-integer value"
		private[this] def keyError:String = tagNumber + " array key not 0 or 1"
		
		override def init:(BigInt, BigInt) = ((BigInt(1), BigInt(1)))
		final def apply[Input](folding:(BigInt, BigInt), key:CborValue, input:Input, parser:Parser[CborValue, CborValue, Input]):Either[(String, Int), (BigInt, BigInt)] = {
			parser.parsePrimitive(input).right.flatMap{_ match {
				case CborValueNumber(x, num) => num.tryToBigInt(x).fold[Either[(String, Int),BigInt]](Left(nonIntError, 0)){x => Right(x)}
				case _ => Left(nonIntError, 0)
			}}.right.flatMap{value =>
				key match {
					case CborValueNumber(x, t) => t.tryToInt(x) match {
						case Some(0) => Right(folding.copy(_1 = value))
						case Some(1) => Right(folding.copy(_2 = value))
						case _ => Left(keyError, 0)
					}
					case _ => Left(keyError, 0)
				}
			}
		}
	}
	
	
	
	/**
	 * The CBOR major types.
	 * Because magic numbers are bad.
	 */
	private[json] object MajorTypeCodes {
		final val POSITIVE_INT:Byte = 0
		final val NEGATIVE_INT:Byte = 1
		final val BYTE_ARRAY:Byte = 2
		final val STRING:Byte = 3
		final val ARRAY:Byte = 4
		final val OBJECT:Byte = 5
		final val TAG:Byte = 6
		final val SPECIAL:Byte = 7
	}
	
	/**
	 * Known simple values.
	 * Because magic numbers are bad.
	 */
	private[json] object SimpleValueCodes {
		final val FALSE:Byte  = 20
		final val TRUE:Byte   = 21
		final val NULL:Byte   = 22
		final val HALF_FLOAT:Byte = 25
		final val FLOAT:Byte  = 26
		final val DOUBLE:Byte = 27
		final val END_OF_LIST:Byte = 31
	}
	
}
