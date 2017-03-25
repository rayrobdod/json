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
import com.rayrobdod.json.union.{CborValue, ParserRetVal}
import com.rayrobdod.json.union.ParserRetVal.{Complex, Primitive, ParserFailure, BuilderFailure}
import com.rayrobdod.json.union.CborValue.Rational
import com.rayrobdod.json.union.Failures.ExpectedPrimitive

/**
 * A parser that will decode cbor data.
 * 
 * This does not support
   - complex values in map keys
   - most tags
 * 
 * tags are handled via the `tagMatcher` constructor parameter. By default, it can handle tags (2,3,4,5,30,55799).
 * 
 * @version 4.0
 * @see [[http://tools.ietf.org/html/rfc7049]]
 * 
 * @constructor
 * Creates a CborParser instance.
 * @param tagMatcher tag support
 */
// TODO: location annotation
final class CborParser(tagMatcher:CborParser.TagMatcher = CborParser.TagMatcher.allBuiltIn) extends Parser[CborValue, CborValue, CborParser.Failures, DataInput] {
	import CborParser._
	import CborParser.Failures._
	
	// used in a match statement; therefore identifier needs to be uppercase
	private[this] val UpperCaseTagMatcher = tagMatcher
	
	override def parse[ComplexOutput, BF](builder:Builder[CborValue, CborValue, BF, ComplexOutput], i:DataInput):ParserRetVal[ComplexOutput, CborValue, CborParser.Failures, BF] = {
		val a = this.parseDetailed[ComplexOutput, BF](builder, i)
		a match {
			case ParseReturnValueSimple(x:CborValue) => Primitive(x)
			case ParseReturnValueComplex(x) => Complex(x)
			case ParseReturnValueParserFailure(x) => ParserFailure(x)
			case ParseReturnValueBuilderFailure(x) => BuilderFailure(x)
			case _ => ParserFailure(NonPublicValue)
		}
	}
	
	
	/**
	 * Decodes the input values to an object.
	 */
	def parseDetailed[ComplexOutput, BF](topBuilder:Builder[CborValue, CborValue, BF, ComplexOutput], input:DataInput):ParseReturnValue[ComplexOutput, BF] = {
		val headerByte:Byte = input.readByte();
		val majorType = (headerByte >> 5) & 0x07
		val additionalInfo = headerByte & 0x1F
		val additionalInfoDataTry = additionalInfo match {
			case x if (x <= 23) => { Right(AdditionalInfoDeterminate(x)) }
			case 24 => { Right(AdditionalInfoDeterminate(input.readUnsignedByte())) }
			case 25 => { Right(AdditionalInfoDeterminate(input.readUnsignedShort())) }
			case 26 => { Right(AdditionalInfoDeterminate(input.readInt().longValue & 0x00000000FFFFFFFFL)) }
			case 27 => { Right(AdditionalInfoDeterminate(input.readLong())) } // todo unsigned long (?)
			case 31 => { Right(AdditionalInfoIndeterminate()) }
			case _  => { Left(IllegalAdditionalInfoField(additionalInfo)) }
		}
		
		additionalInfoDataTry.fold({x =>
			new ParseReturnValueParserFailure(x)
		}, {additionalInfoData =>
			majorType match {
				// positive integer
				case MajorTypeCodes.POSITIVE_INT => additionalInfoData match {
					case AdditionalInfoDeterminate(value) => ParseReturnValueSimple(CborValue( value ))
					case x:AdditionalInfoIndeterminate => ParseReturnValueParserFailure(IndeterminateIntegerValue)
				}
				// negative integer
				case MajorTypeCodes.NEGATIVE_INT => additionalInfoData match {
					case AdditionalInfoDeterminate(value) => ParseReturnValueSimple(CborValue( -1 - value ))
					case x:AdditionalInfoIndeterminate => ParseReturnValueParserFailure(IndeterminateIntegerValue)
				}
				// byte string
				case MajorTypeCodes.BYTE_ARRAY => parseByteString(input, additionalInfoData) match {
					case Right(x) => ParseReturnValueSimple(CborValue(x))
					case Left(x) => ParseReturnValueParserFailure(x)
				}
				// text string
				case MajorTypeCodes.STRING => parseByteString(input, additionalInfoData) match {
					case Right(x) => ParseReturnValueSimple(CborValue(new String(x, UTF_8)))
					case Left(x) => ParseReturnValueParserFailure(x)
				}
				// array/list
				case MajorTypeCodes.ARRAY => parseArray(topBuilder, input, additionalInfoData) match {
					case Complex(x) => ParseReturnValueComplex(x)
					case ParserFailure(x) => ParseReturnValueParserFailure(x)
					case BuilderFailure(x) => ParseReturnValueBuilderFailure(x)
					case ParserRetVal.Primitive(x) => x:Nothing
				}
				// map
				case MajorTypeCodes.OBJECT => parseObject(topBuilder, input, additionalInfoData) match {
					case Complex(x) => ParseReturnValueComplex(x)
					case ParserFailure(x) => ParseReturnValueParserFailure(x)
					case BuilderFailure(x) => ParseReturnValueBuilderFailure(x)
					case ParserRetVal.Primitive(x) => x:Nothing
				}
				// tags
				case MajorTypeCodes.TAG => additionalInfoData match {
					case AdditionalInfoDeterminate(value) => value match {
						case UpperCaseTagMatcher(fun) => fun.apply(topBuilder, input)
						case _ => new ParseReturnValueTaggedValue(value, this.parseDetailed(topBuilder, input))
					}
					case x:AdditionalInfoIndeterminate => ParseReturnValueParserFailure(IndeterminateTagValue)
				}
				// floats/simple
				case MajorTypeCodes.SPECIAL => additionalInfo match {
					case SimpleValueCodes.FALSE => ParseReturnValueSimple(CborValue( false ))
					case SimpleValueCodes.TRUE  => ParseReturnValueSimple(CborValue( true ))
					case SimpleValueCodes.NULL => ParseReturnValueSimple(CborValue.CborValueNull)
					case SimpleValueCodes.HALF_FLOAT => additionalInfoData match {
						case AdditionalInfoDeterminate(value) => ParseReturnValueSimple(CborValue(Rational.fromHalfFloat(value.shortValue)))
						case x:AdditionalInfoIndeterminate => ParseReturnValueParserFailure(ThingsThatShouldBeImpossible("Indeterminate half-float"))
					}
					case SimpleValueCodes.FLOAT => additionalInfoData match {
						case AdditionalInfoDeterminate(value) => ParseReturnValueSimple(CborValue( java.lang.Float.intBitsToFloat(value.intValue)))
						case x:AdditionalInfoIndeterminate => ParseReturnValueParserFailure(ThingsThatShouldBeImpossible("Indeterminate float"))
					}
					case SimpleValueCodes.DOUBLE => additionalInfoData match {
						case AdditionalInfoDeterminate(value) => ParseReturnValueSimple(CborValue( java.lang.Double.longBitsToDouble(value.longValue)))
						case x:AdditionalInfoIndeterminate => ParseReturnValueParserFailure(ThingsThatShouldBeImpossible("Indeterminate double"))
					}
					case SimpleValueCodes.END_OF_LIST => new ParseReturnValueEndOfIndeterminateObject
					case _  => additionalInfoData match {
						case AdditionalInfoDeterminate(value) => ParseReturnValueUnknownSimple(value.byteValue)
						case x:AdditionalInfoIndeterminate => ParseReturnValueParserFailure(IndeterminateSpecialValue)
					}
				}
				// `whatver & 7` can only be a value between 0 through 7 inclusive, but
				// scala's type system does not know that, hence this unreachable statement.
				case _ => ParseReturnValueParserFailure(ThingsThatShouldBeImpossible("majorType was greater than 7"))
			}
		})
	}
	
	private[this] def parseByteString(input:DataInput, aid:AdditionalInfoData):Either[CborParser.Failures, Array[Byte]] = {
		final class WrongStringTypeException extends Exception
		
		try {
			aid match {
				case AdditionalInfoIndeterminate() => {
					val stream = new java.io.ByteArrayOutputStream
					
					var next:Any = this.parseDetailed(new PrimitiveSeqBuilder, input)
					while (next != ParseReturnValueEndOfIndeterminateObject()) {
						val nextBytes:Array[Byte] = next match {
							case ParseReturnValueSimple(CborValue.CborValueString(s:String)) => s.getBytes(UTF_8)
							case ParseReturnValueSimple(CborValue.CborValueByteStr(a:Array[Byte])) => a
							case _ => throw new WrongStringTypeException
						}
						stream.write(nextBytes)
						next = this.parseDetailed(new PrimitiveSeqBuilder, input)
					}
					Right(stream.toByteArray())
				}
				case AdditionalInfoDeterminate(len:Long) => {
					if (len.isValidInt) {
						val bytes = new Array[Byte](len.intValue)
						input.readFully(bytes)
						Right(bytes)
					} else {
						Left(StringLengthTooLong(len))
					}
				}
			}
		} catch {
			case ex:java.io.IOException => Left(IOException(ex.getMessage()))
			case ex:WrongStringTypeException => Left(IndeterminateStringSubstringsNotStrings)
		}
	}
	
	private[this] def parseArray[A,BF](topBuilder:Builder[CborValue, CborValue, BF, A], input:DataInput, aid:AdditionalInfoData):ParserRetVal[A, Nothing, CborParser.Failures, BF] = {
		var retVal:ParserRetVal[topBuilder.Middle, Nothing, CborParser.Failures, BF] = Complex(topBuilder.init)
		
		aid match {
			case AdditionalInfoDeterminate(len:Long) => {
				(0L until len).foreach{index =>
					retVal = retVal.complex.flatMap{x => topBuilder.apply[DataInput, CborParser.Failures](x, CborValue(index), input, this)}
				}
			}
			case AdditionalInfoIndeterminate() => {
				var index:Int = 0
				var childObject:ParseReturnValue[Seq[Byte], com.rayrobdod.json.union.Failures.IllegalFoldingInBuilder.type] = ParseReturnValueUnknownSimple(0)
				while (childObject != ParseReturnValueEndOfIndeterminateObject()) {
					childObject = this.parseDetailed(new CborBuilder(true), input)
					
					childObject match {
						case ParseReturnValueEndOfIndeterminateObject() => {}
						case ParseReturnValueSimple(x) => {
							retVal = retVal.complex.flatMap{y => topBuilder.apply[CborValue, Nothing](y, CborValue(index), x, new IdentityParser())}
						}
						case ParseReturnValueComplex(x) => {
							retVal = retVal.complex.flatMap{y => topBuilder.apply[DataInput, CborParser.Failures](y, CborValue(index), byteArray2DataInput(x.toArray), this)}
						}
						case ParseReturnValueParserFailure(x) => {
							retVal = ParserFailure(x)
						}
						case ParseReturnValueBuilderFailure(x) => {
							retVal = ParserFailure(ThingsThatShouldBeImpossible("CborParser used a CborBuilder incorrectly"))
						}
						case _ => retVal = ParserFailure(NonPublicValue)
					}
					index = index + 1
				}
			}
		}
		retVal
			.complex.flatMap{topBuilder.finalize _}
	}
	
	private[this] def parseObject[A,BF](topBuilder:Builder[CborValue, CborValue, BF, A], input:DataInput, aid:AdditionalInfoData):ParserRetVal[A, Nothing, CborParser.Failures, BF] = {
		var retVal:ParserRetVal[topBuilder.Middle, Nothing, CborParser.Failures, BF] = Complex(topBuilder.init)
		
		aid match {
			case AdditionalInfoDeterminate(len:Long) => {
				(0L until len).foreach{index =>
					val keyTry:ParserRetVal[CborValue, Nothing, CborParser.Failures, Nothing] = this.parseDetailed(new ThrowBuilder, input) match {
						case ParseReturnValueSimple(x) => Complex(x)
						case ParseReturnValueParserFailure(x) => ParserFailure(x)
						case ParseReturnValueBuilderFailure(x) => ParserFailure(NonSimpleMapKey)
						case _ => ParserFailure(NonSimpleMapKey)
					}
					retVal = for (
						foldingObject <- retVal.complex;
						keyObject <- keyTry.complex;
						newRetVal <- topBuilder.apply[DataInput, CborParser.Failures](foldingObject, keyObject, input, this).complex
					) yield {newRetVal}
				}
			}
			case AdditionalInfoIndeterminate() => {
				var keyObject:ParseReturnValue[_, _] = ParseReturnValueUnknownSimple(0)
				while (keyObject != ParseReturnValueEndOfIndeterminateObject()) {
					keyObject = this.parseDetailed(new ThrowBuilder, input)
					keyObject match {
						case ParseReturnValueEndOfIndeterminateObject() => {}
						case ParseReturnValueSimple(x) => {
							retVal = retVal.complex.flatMap{y => topBuilder.apply[DataInput, CborParser.Failures](y, x, input, this)}
						}
						case ParseReturnValueParserFailure(x) => retVal = ParserFailure(x)
						case ParseReturnValueBuilderFailure(x) => retVal = ParserFailure(NonSimpleMapKey)
						case _ => retVal = ParserFailure(NonSimpleMapKey)
					}
				}
			}
		}
		retVal
			.complex.flatMap{topBuilder.finalize _}
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
	sealed trait ParseReturnValue[+A, +BF]
	final case class ParseReturnValueSimple(x:CborValue) extends ParseReturnValue[Nothing, Nothing]
	final case class ParseReturnValueComplex[A](a:A) extends ParseReturnValue[A, Nothing]
	/**
	 * The marker of the end of an indeterminate value. Represented as (0xFF).
	 * Unless you're trying to see this value, you shouldn't see this value.
	 */
	final case class ParseReturnValueEndOfIndeterminateObject() extends ParseReturnValue[Nothing, Nothing]
	/** An unknown tagged value */
	final case class ParseReturnValueTaggedValue[A, BF](tag:Long, x:ParseReturnValue[A, BF]) extends ParseReturnValue[A, BF]
	/** A simple value other than the known ones */
	final case class ParseReturnValueUnknownSimple(value:Byte) extends ParseReturnValue[Nothing, Nothing]
	final case class ParseReturnValueParserFailure(err:Failures) extends ParseReturnValue[Nothing, Nothing]
	final case class ParseReturnValueBuilderFailure[BF](err:BF) extends ParseReturnValue[Nothing, BF]
	
	
	/**
	 * Possible failures that can occur in a CborParser
	 * @since 4.0
	 */
	sealed trait Failures
	/**
	 * Possible failures that can occur in a CborParser
	 * @since 4.0
	 */
	object Failures {
		/** The value parsed is not a value that is suposed to be public */
		final object NonPublicValue extends Failures
		/** The AdditionalInfo field is an illegal value */
		final case class IllegalAdditionalInfoField(value:Int) extends Failures
		/** A major-type 0 or 1's additional info indicated that it is indeterminate */
		final object IndeterminateIntegerValue extends Failures
		final object IndeterminateSpecialValue extends Failures
		final object IndeterminateTagValue extends Failures
		final case class IOException(msg:String) extends Failures
		final object IndeterminateStringSubstringsNotStrings extends Failures
		final case class StringLengthTooLong(value:Long) extends Failures
		object NonSimpleMapKey extends Failures
		
		final case class ThingsThatShouldBeImpossible(msg:String) extends Failures
		
		class TagFailures extends Failures
		final case class BigIntPairTagHadKeysOtherThanZeroOrOne(tag:String, key:CborValue) extends TagFailures
		final case class BigIntPairTagHadValuesOtherthanBigInts(tag:String, key:CborValue) extends TagFailures
		final case class BigIntPairTagHadComplexValue(tag:String) extends TagFailures
		final case class BigIntTagContainedNonByteStringValue(tag:String, value:CborValue) extends TagFailures
		final case class BigIntPairTagHadPrimitiveValue(tag:String, value:CborValue) extends TagFailures
	}
	
	/**
	 * A function that is parameterized at the function level instead of the class level
	 * @since 3.1
	 */
	trait TagFunction {
		def apply[A, BF](b:Builder[CborValue, CborValue, BF, A], i:DataInput):ParseReturnValue[A, BF] 
	}
	
	/**
	 * A partial function that takes a Cbor tag number and returns a function that builds the value described by the tags
	 * @since 3.1
	 */
	trait TagMatcher {
		def unapply(tag:Long):Option[TagFunction]
		
		/**
		 * Composes this partial function with a fallback partial function which gets applied where this partial function is not defined. 
		 */
		final def orElse(rhs:TagMatcher):TagMatcher = new TagMatcher {
			override def unapply(tag:Long):Option[TagFunction] = {
				TagMatcher.this.unapply(tag).orElse(rhs.unapply(tag))
			}
		}
	}
	
	/**
	 * Built-in TagMatchers
	 * @since 3.1
	 */
	object TagMatcher {
		
		/** a TagMatcher that maches no tags (and thus always returns None) */
		val empty:TagMatcher = new TagMatcher {
			def unapply(tag:Long):Option[Nothing] = None
		}
		
		
		/** The self-describe tag (55799) */
		val selfDescribe:TagMatcher = new TagMatcher {
			def unapply(tag:Long):Option[TagFunction] = tag match {
				case TagCodes.SELF_DESCRIBE => Some(new TagFunction{override def apply[A, BF](b:Builder[CborValue, CborValue, BF, A], i:DataInput) = {
					new CborParser().parseDetailed(b, i)
				}})
				case _ => None
			}
		}
		
		import CborValue._
		import scala.math.{BigDecimal, BigInt}
		import CborParser.Failures._
		/** the tags indicating extended number formats; (2, 3, 4, 5, 30) */
		val numbers:TagMatcher = new TagMatcher {
			def unapply(tag:Long):Option[TagFunction] = tag match {
				case TagCodes.POS_BIG_INT => Some(new TagFunction{override def apply[A, BF](b:Builder[CborValue, CborValue, BF, A], i:DataInput) = {
					val bsOpt = new CborParser().parsePrimitive(i)
					bsOpt.primitive.flatMap{_ match {
						case CborValueByteStr(bs)=> Complex(CborValueNumber(Rational(bs.foldLeft(0:BigInt){(a,b) => (a * 0x100) + ((b:Int) & (0xFF))})))
						case x => ParserFailure(BigIntTagContainedNonByteStringValue("Tag 2", x))
					}}.fold(
						{x => ParseReturnValueSimple(x)},
						{x:Nothing => x},
						{x => ParseReturnValueParserFailure(x)},
						{x:ExpectedPrimitive.type => ParseReturnValueParserFailure(BigIntPairTagHadComplexValue("Tag 2"))}
					)
				}})
				case TagCodes.NEG_BIG_INT => Some(new TagFunction{override def apply[A, BF](b:Builder[CborValue, CborValue, BF, A], i:DataInput) = {
					val bsOpt = new CborParser().parsePrimitive(i)
					bsOpt.primitive.flatMap{_ match {
						case CborValueByteStr(bs)=> Complex(CborValueNumber(Rational((-1:BigInt) - bs.foldLeft(0:BigInt){(a,b) => (a * 0x100) + ((b:Int) & (0xFF))})))
						case x => ParserFailure(BigIntTagContainedNonByteStringValue("Tag 3", x))
					}}.fold(
						{x => ParseReturnValueSimple(x)},
						{x:Nothing => x},
						{x => ParseReturnValueParserFailure(x)},
						{x:ExpectedPrimitive.type => ParseReturnValueParserFailure(BigIntPairTagHadComplexValue("Tag 2"))}
					)
				}})
				case TagCodes.BIG_DECIMAL => Some(new TagFunction{override def apply[A, BF](b:Builder[CborValue, CborValue, BF, A], i:DataInput) = {
					new CborParser().parse(new PairBigIntBuilder("Tag 4"), i).fold(
						{x => val (exp,mant) = x; ParseReturnValueSimple(CborValueNumber(Rational(BigDecimal(mant) * BigDecimal(10).pow(exp.intValue))))},
						{x => ParseReturnValueParserFailure(BigIntPairTagHadPrimitiveValue("Tag 4", x))},
						{x => ParseReturnValueParserFailure(x)},
						{x => ParseReturnValueParserFailure(x)}
					)
				}})
				case TagCodes.BIG_FLOAT => Some(new TagFunction{override def apply[A, BF](b:Builder[CborValue, CborValue, BF, A], i:DataInput) = {
					new CborParser().parse(new PairBigIntBuilder("Tag 5"), i).fold(
						{x => val (exp,mant) = x; ParseReturnValueSimple(CborValueNumber(Rational(BigDecimal(mant) * BigDecimal(2).pow(exp.intValue))))},
						{x => ParseReturnValueParserFailure(BigIntPairTagHadPrimitiveValue("Tag 5", x))},
						{x => ParseReturnValueParserFailure(x)},
						{x => ParseReturnValueParserFailure(x)}
					)
				}})
				case TagCodes.RATIONAL => Some(new TagFunction{override def apply[A, BF](b:Builder[CborValue, CborValue, BF, A], i:DataInput) = {
					new CborParser().parse(new PairBigIntBuilder("Tag 30"), i).fold(
						{x => val (a,b) = x; ParseReturnValueSimple(new Rational(a,b))},
						{x => ParseReturnValueParserFailure(BigIntPairTagHadPrimitiveValue("Tag 30", x))},
						{x => ParseReturnValueParserFailure(x)},
						{x => ParseReturnValueParserFailure(x)}
					)
				}})
				case _ => None
			}
		}
		
		private[this] final class PairBigIntBuilder(tagNumber:String) extends Builder[CborValue, CborValue, CborParser.Failures, (BigInt, BigInt)] {
			override type Middle = Tuple2[BigInt, BigInt]
			override def init:(BigInt, BigInt) = ((BigInt(1), BigInt(1)))
			override def apply[Input, PF](folding:(BigInt, BigInt), key:CborValue, input:Input, parser:Parser[CborValue, CborValue, PF, Input]):ParserRetVal[(BigInt, BigInt), Nothing, PF, CborParser.Failures] = {
				parser.parsePrimitive(input)
					.builderFailure.map[CborParser.Failures]{x:ExpectedPrimitive.type => BigIntPairTagHadComplexValue(tagNumber)}
					.primitive.flatMap[BigInt, Nothing, PF, CborParser.Failures]{_ match {
						case CborValueNumber(x) => (
							x.tryToBigInt.fold[ParserRetVal[BigInt, Nothing, PF, CborParser.Failures]]
								(BuilderFailure(BigIntPairTagHadValuesOtherthanBigInts(tagNumber, x)))
								{x => Complex(x)}
						)
						case x => BuilderFailure(BigIntPairTagHadValuesOtherthanBigInts(tagNumber, x))
					}}
					.complex.flatMap{value =>
						key match {
							case CborValueNumber(x) => x.tryToInt match {
								case Some(0) => Complex(folding.copy(_1 = value))
								case Some(1) => Complex(folding.copy(_2 = value))
								case _ => BuilderFailure(BigIntPairTagHadKeysOtherThanZeroOrOne(tagNumber, x))
							}
							case x => BuilderFailure(BigIntPairTagHadKeysOtherThanZeroOrOne(tagNumber, x))
						}
					}
			}
			override def finalize(folding:Tuple2[BigInt, BigInt]):ParserRetVal.Complex[Tuple2[BigInt, BigInt]] = ParserRetVal.Complex(folding)
		}
		
		/** Combines the other tag matchers  */
		val allBuiltIn:TagMatcher = selfDescribe.orElse(numbers)
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
	
	/**
	 * Known tag codes.
	 * Because magic numbers are bad.
	 */
	private[json] object TagCodes {
		final val POS_BIG_INT:Byte = 2
		final val NEG_BIG_INT:Byte = 3
		final val BIG_DECIMAL:Byte = 4
		final val BIG_FLOAT:Byte = 5
		final val RATIONAL:Byte = 30
		final val SELF_DESCRIBE:Int = 55799
	}
	
}
