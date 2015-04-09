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
 * 
 */
final class BsonParser[A](topBuilder:Builder[A]) {
	private val END_OF_DOCUMENT_TYPE:Byte = 0x00;
	
	
	/**
	 * Decodes the input values to an object.
	 */
	def parse(input:DataInput):A = {
		// We don't really care about the document length.
		/* val length = */ Integer.reverseBytes( input.readInt() );	
		
		var result:A = topBuilder.init
		var valueType:Byte = input.readByte();
		while (valueType != END_OF_DOCUMENT_TYPE) {
			val key:String = readCString(input)
			val value = valueType match {
				case  1 => { // float
					java.lang.Double.longBitsToDouble(
						java.lang.Long.reverseBytes( input.readLong() )
					)
				}
				case  2 => { // string
					val len = Integer.reverseBytes( input.readInt() );
					val bytes = new Array[Byte](len);
					input.readFully(bytes);
					if (bytes(len - 1) != 0) {throw new ParseException("Incorrect string length", -1)}
					new String(bytes, 0, len - 1, UTF_8)
				}
				case  3 => { // document
					new BsonParser(topBuilder.childBuilder(key)).parse(input)
				}
				case  4 => { // document (array)
					new BsonParser(topBuilder.childBuilder(key)).parse(input)
				}
				case  8 => { // boolean
					val readValue = input.readByte()
					(readValue != 0)
				}
				case 10 => null
				case 16 => Integer.reverseBytes( input.readInt() );
				case 18 => java.lang.Long.reverseBytes( input.readLong() );
				case _ => throw new ParseException("Unknown data type", -1)
			}
			
			result = topBuilder.apply(result, key, value);
			valueType = input.readByte();
		}
		
		result
	}
	
	/* def parse(is:InputStream):A = this.parse(
		new java.io.DataInputStream(ba)
	)
	
	def parse(ba:Array[Byte]):A = this.parse(
		new java.io.ByteArrayInputStream(ba)
	) */
	
	
	
	
	
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
		
		return new String( data.toByteArray(), UTF_8);
	}
}
