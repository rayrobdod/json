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

import java.text.ParseException;
import scala.collection.immutable.Map;
import org.scalatest.FunSpec;
import com.rayrobdod.json.builder.MapBuilder;

class BsonParserTest_UnHappy extends FunSpec {
	describe("BsonParser (Unhappy)") {
		it ("String is longer than prefix") {
			val src = byteArray2DataInput(
					Array[Byte](0,0,0,0,
						0x02,0,  2,0,0,0,  'a','b','c',
					0)
			);
			
			intercept[ParseException] {
				new BsonParser().parseComplex(new MapBuilder(), src)
			}
		}
		it ("String is shorter than prefix") {
			val src = byteArray2DataInput(
					Array[Byte](0,0,0,0,
						0x02,0,  2,0,0,0,  'a',
						0x02,0,  2,0,0,0,  'a','b',
					0)
			);
			
			intercept[ParseException] {
				new BsonParser().parseComplex(new MapBuilder(), src)
			}
		}
		it ("data ends early") {
			val src = byteArray2DataInput(
					Array[Byte](0,0,0,0,
						0x02,0,  2,0)
			);
			
			intercept[java.io.EOFException] {
				new BsonParser().parseComplex(new MapBuilder(), src)
			}
		}
		it ("Does not parse on unknown data type") {
			val src = byteArray2DataInput(
					Array[Byte](0,0,0,0,
						0x50,0,  3,0,0,0,  'a','b','c',
					0)
			);
			
			intercept[ParseException] {
				new BsonParser().parseComplex(new MapBuilder(), src)
			}
		}
	}
}
