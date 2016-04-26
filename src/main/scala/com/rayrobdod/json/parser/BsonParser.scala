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
import com.rayrobdod.json.builder._
import com.rayrobdod.json.union._

/**
 * A streaming parser for Bson values
 * 
 * 
 * @see [[http://bsonspec.org/]]
 *
 * @constructor
 * Creates a BsonParser instance.
 */
final class BsonParser extends Parser[String, JsonValue, DataInput] {
	import BsonParser.{readCString, TypeCodes}
	
	def parse[A](builder:Builder[String, JsonValue, A], input:DataInput):Either[A,JsonValue] = Left{
		// We don't really care about the document length.
		/* val length = */ Integer.reverseBytes( input.readInt() );
		
		var result:A = builder.init
		var valueType:Byte = input.readByte();
		while (valueType != TypeCodes.END_OF_DOCUMENT) {
			val key:String = readCString(input)
			result = valueType match {
				case TypeCodes.FLOAT => {
					val value = java.lang.Double.longBitsToDouble(
						java.lang.Long.reverseBytes( input.readLong() )
					)
					// CHEATING
					builder.apply(key).apply(result, JsonValue(value), new IdentityParser[String, JsonValue])
				}
				case TypeCodes.STRING => {
					val len = Integer.reverseBytes( input.readInt() );
					val bytes = new Array[Byte](len);
					input.readFully(bytes);
					if (bytes(len - 1) != 0) {throw new ParseException("Incorrect string length", -1)}
					val value = new String(bytes, 0, len - 1, UTF_8)
					builder.apply(key).apply(result, JsonValue(value), new IdentityParser[String, JsonValue])
				}
				case TypeCodes.DOCUMENT => {
					builder.apply(key).apply(result, input, this)
				}
				case TypeCodes.ARRAY => {
					builder.apply(key).apply(result, input, this)
				}
				case TypeCodes.BOOLEAN => {
					val readValue = input.readByte()
					val value = (readValue != 0)
					builder.apply(key).apply(result, JsonValue(value), new IdentityParser[String, JsonValue])
				}
				case TypeCodes.NULL => {
					builder.apply(key).apply(result, JsonValue.JsonValueNull, new IdentityParser[String, JsonValue])
				}
				case TypeCodes.INTEGER => {
					val value = Integer.reverseBytes( input.readInt() );
					builder.apply(key).apply(result, JsonValue(value), new IdentityParser[String, JsonValue])
				}
				case TypeCodes.LONG => {
					val value = java.lang.Long.reverseBytes( input.readLong() );
					builder.apply(key).apply(result, JsonValue(value), new IdentityParser[String, JsonValue])
				}
				case _ => throw new ParseException("Unknown data type", -1)
			}
			
			valueType = input.readByte();
		}
		
		result
	}
}


private object BsonParser {
	/**
	 * Reads a c-style string from the DataInput.
	 * Basically, reads things until it reaches a '0x00' and then throws what it read into a String.
	 * @throws IOException
	 */
	private def readCString(input:DataInput):String = {
		val data = new java.io.ByteArrayOutputStream();
		
		var byte = input.readByte();
		while (byte != 0x00) {
			data.write(byte);
			byte = input.readByte();
		}
		
		new String( data.toByteArray(), UTF_8);
	}
	
	
	/** because magic numbers are bad */
	object TypeCodes {
		val END_OF_DOCUMENT = 0
		val FLOAT = 1
		val STRING = 2
		val DOCUMENT = 3
		val ARRAY = 4
		val BOOLEAN = 8
		val NULL = 10
		val INTEGER = 16
		val LONG = 18
	}
}
