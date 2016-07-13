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
import com.rayrobdod.json.parser.CborParser.{MajorTypeCodes, SimpleValueCodes}
import com.rayrobdod.json.union.CborValue
import com.rayrobdod.json.union.ParserRetVal
import com.rayrobdod.json.parser.{Parser, CborParser, byteArray2DataInput}

/**
 * A builder that will create a series of bytes in Cbor Object format
 * 
 * @since 3.0
 * @constructor
 * A builder that will create cbor object format byte strings
 * @param forceObject true if the builder should create an object even if it is possible to create an array from the inputs
 */
final class CborBuilder(forceObject:Boolean = false) extends Builder[CborValue, CborValue, Seq[Byte]] {
	import CborObjectBuilder._
	import ParserRetVal._
	
	/** The bytes to encode a zero-length array or object  */
	override val init:Seq[Byte] = encodeLength((if (forceObject) {MajorTypeCodes.OBJECT} else {MajorTypeCodes.ARRAY}), 0)
	
	override def apply[Input](folding:Seq[Byte], key:CborValue, input:Input, parser:Parser[CborValue, CborValue, Input]):Either[(String, Int), Seq[Byte]] = {
		val value = parser.parse[Seq[Byte]](this, input)
		val encodedValueOpt = value match {
			case Failure(a,b) => Left((a,b))
			case Complex(x) => Right(x)
			case Primitive(x) => Right(encodeValue(x))
		}
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
					case CborValue.CborValueNumber(x) if (x == objectLength) => {
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