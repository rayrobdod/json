/*
	Copyright (c) 2013, Raymond Dodge
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
package com.rayrobdod.testing.binaryJSON.parser.listeners

/**
 * @author Raymond Dodge
 * @version 2013 Aug 03
 */
object commonObjects {
	def DataInput(bytes:Array[Byte]) = {
		new java.io.DataInputStream(
			new java.io.ByteArrayInputStream( bytes ))
	}
	
	
	/** {} */
	def empty = DataInput(
		Array[Byte](0,0,0,0,0)
	)
	
	/** {"hello":"world"} */
	def helloWorld = DataInput(
		Array[Byte](
			0x16, 0x00, 0x00, 0x00, 0x02, 0x68, 0x65, 0x6C, 0x6C, 0x6F, 0x00,
			0x06, 0x00, 0x00, 0x00, 0x77, 0x6F, 0x72, 0x6C, 0x64, 0x00, 0x00 
		)
	)
	
	def countTo20 = {
		val elements:Seq[Seq[Byte]] = (0 until 10).map{(i:Int) =>
				Seq[Byte](0x10, (0x30 + i).byteValue, 0x00, i.byteValue, 0x00, 0x00, 0x00) 
			} ++: (10 until 20).map{(i:Int) =>
				Seq[Byte](0x10, 0x31, (0x30 + i).byteValue, 0x00, i.byteValue, 0x00, 0x00, 0x00) 
			}
		val flattenedElements:Seq[Byte] = elements.flatten
		val elementsArray:Array[Byte] = flattenedElements.toArray
		
		val len = elementsArray.length + 1;
		
		DataInput(
			// todo: figure out actual length of the thing
			Array[Byte](len.byteValue, 0x00, 0x00, 0x00) ++: elementsArray :+ 0x00.byteValue
		)
	}
	
	/** {"BSON": ["awesome", 5.05, 1986]} */
	def specSample = DataInput(
		Array[Int](
			0x31, 0x00, 0x00, 0x00,
				0x04, 'B', 'S', 'O', 'N', 0x00, 0x26, 0x00, 0x00, 0x00,
					0x02, '0', 0x00, 0x08, 0x00, 0x00, 0x00,
							'a', 'w', 'e', 's', 'o', 'm', 'e', 0x00,
					0x01, '1', 0x00, 0x33, 0x33, 0x33, 0x33, 0x33, 0x33, 0x14, 0x40,
					0x10, '2', 0x00, 0xC2, 0x07, 0x00, 0x00,
				0x00,
			0x00
		).map{_.byteValue}
	)
	
	/** ["\0"] */
	def containsNul = DataInput(
		Array[Int](
			14,0,0,0,
				0x02,'0',0, 2,0,0,0, 0,0, 
			0
		).map{_.byteValue}
	)
	
	/** [[1,2]] */
	def recursion = DataInput(
		Array[Int](
			27,0,0,0,
				0x04,'0',0, 19,0,0,0,
					0x10,'0',0, 1,0,0,0,
					0x10,'1',0, 2,0,0,0,
				0,
			0
		).map{_.byteValue}
	)
}


object EmptyDataInput extends java.io.DataInput {
	val readBoolean:Boolean = false;
	val readByte:Byte = 0;
	val readChar:Char = 0;
	val readDouble:Double = 0;
	val readFloat:Float = 0;
	def readFully(b:Array[Byte]) {}
	def readFully(b:Array[Byte], off:Int, len:Int) {}
	val readInt:Int = 0;
	val readLine:String = "";
	val readLong:Long = 0;
	val readShort:Short = 0;
	val readUnsignedByte:Int = 0;
	val readUnsignedShort:Int = 0;
	val readUTF:String = "";
	def skipBytes(n:Int):Int = 0;
}
