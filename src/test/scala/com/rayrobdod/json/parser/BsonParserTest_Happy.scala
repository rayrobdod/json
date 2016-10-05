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

import scala.collection.immutable.Map;
import org.scalatest.FunSpec;
import com.rayrobdod.json.builder.MapBuilder;
import com.rayrobdod.json.union.CborValue;

class BsonParserTest_Happy extends FunSpec {

	private val testValues:Seq[(String, Array[Byte], Map[String,Either[Any, CborValue]], Boolean)] = Seq(
		("empty object", Array[Byte](0,0,0,0,0), Map.empty, false),
		("object containing one null", Array[Byte](2,0,0,0,
						0x0A, 0,
					0), Map("" -> Right(CborValue.CborValueNull)), false),
		("object containing one int", Array[Byte](5,0,0,0,
						0x10, 0, 0x78, 0x56, 0x34, 0x12,
					0), Map("" -> Right(CborValue(0x12345678))), false),
		("object containing one long", Array(5,0,0,0,
						0x12, 0, 0xEF, 0xCD, 0xAB, 0x90, 0x78, 0x56, 0x34, 0x12,
					0).map{_.byteValue}, Map("" -> Right(CborValue(0x1234567890ABCDEFl))), false),
		("object containing one false", Array[Byte](5,0,0,0,
						0x08, 0, 0,
					0), Map("" -> Right(CborValue(false))), false),
		("object containing one true", Array[Byte](5,0,0,0,
						0x08, 0, 1,
					0), Map("" -> Right(CborValue(true))), false),
		("The hello world object",
						Array[Byte](
							0x16, 0x00, 0x00, 0x00, 0x02, 0x68, 0x65, 0x6C, 0x6C, 0x6F, 0x00,
							0x06, 0x00, 0x00, 0x00, 0x77, 0x6F, 0x72, 0x6C, 0x64, 0x00, 0x00 
						), Map("hello" -> Right(CborValue("world"))), false),
		("The specs sample document",
						Array[Int](
							0x31, 0x00, 0x00, 0x00,
								0x04, 'B', 'S', 'O', 'N', 0x00, 0x26, 0x00, 0x00, 0x00,
									0x02, '0', 0x00, 0x08, 0x00, 0x00, 0x00,
											'a', 'w', 'e', 's', 'o', 'm', 'e', 0x00,
									0x01, '1', 0x00, 0x33, 0x33, 0x33, 0x33, 0x33, 0x33, 0x14, 0x40,
									0x10, '2', 0x00, 0xC2, 0x07, 0x00, 0x00,
								0x00,
							0x00
						).map{_.byteValue}, Map("BSON" -> Left(Map("0" -> Right(CborValue("awesome")), "1" -> Right(CborValue(5.05)), "2" -> Right(CborValue(1986))))), true),
		("value contains Nul", Array[Int](14,0,0,0,
							0x02,'0',0, 2,0,0,0, 0,0,
						0).map{_.byteValue}, Map("0" -> Right(CborValue("\u0000"))), false),
		("Object containing object", Array[Int](
					27,0,0,0,
						0x04,'0',0, 19,0,0,0,
							0x10,'0',0, 1,0,0,0,
							0x10,'1',0, 2,0,0,0,
						0,
					0
				).map{_.byteValue}, Map("0" -> Left(Map("0" -> Right(CborValue(1)), "1" -> Right(CborValue(2))))), true),
		("Object containing two objects", Array[Int](
					27,0,0,0,
						0x04,'0',0, 19,0,0,0,
							0x10,'0',0, 1,0,0,0,
							0x10,'1',0, 2,0,0,0,
						0,
						0x04,'1',0, 19,0,0,0,
							0x10,'0',0, 3,0,0,0,
							0x10,'1',0, 4,0,0,0,
						0,
					0
				).map{_.byteValue}, Map("0" -> Left(Map("0" -> Right(CborValue(1)), "1" -> Right(CborValue(2)))), "1" -> Left(Map("0" -> Right(CborValue(3)), "1" -> Right(CborValue(4))))), true),
		("Object containing two objects (0x03)", Array[Int](
					27,0,0,0,
						0x03,'0',0, 19,0,0,0,
							0x10,'0',0, 1,0,0,0,
							0x10,'1',0, 2,0,0,0,
						0,
						0x03,'1',0, 19,0,0,0,
							0x10,'0',0, 3,0,0,0,
							0x10,'1',0, 4,0,0,0,
						0,
					0
				).map{_.byteValue}, Map("0" -> Left(Map("0" -> Right(CborValue(1)), "1" -> Right(CborValue(2)))), "1" -> Left(Map("0" -> Right(CborValue(3)), "1" -> Right(CborValue(4))))), true)
	)
	
	
	
	describe("BsonParser + MapBuilder can decode") {
		testValues.foreach{abc =>
			val (name:String, source2:Array[Byte], expected:Map[String,Any], nesting:Boolean) = abc
			
			if (nesting) {
				val builder = MapBuilder({a:String => MapBuilder.apply[String, CborValue]})
				
				it (name) {
					val source = new java.io.DataInputStream(
						new java.io.ByteArrayInputStream(
							source2
						)
					)
					val result = new BsonParser().parse(builder, source).fold({x => x},{x => x},{(a,b) => a})
					assertResult(expected){result}
				}
			} else {
				val builder = MapBuilder.apply[String, CborValue]
				
				it (name) {
					val source = new java.io.DataInputStream(
						new java.io.ByteArrayInputStream(
							source2
						)
					)
					val result = new BsonParser().parse(builder, source).fold({x => x},{x => x},{(a,b) => a})
					assertResult(expected){result}
				}
			}
		}
	}
}
