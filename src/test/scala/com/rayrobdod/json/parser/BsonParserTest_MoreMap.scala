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

import org.scalatest.FunSpec;
import com.rayrobdod.json.builder.MapBuilder;
import com.rayrobdod.json.union.CborValue;

class BsonParserTest_Happy2 extends FunSpec {
	describe("BsonParser + MapBuilder can decode") {
		it ("20-element list") {
			val elements:Seq[Seq[Byte]] = (0 until 10).map{(i:Int) =>
					Seq[Byte](0x10, (0x30 + i).byteValue, 0x00, i.byteValue, 0x00, 0x00, 0x00) 
				} ++: (10 until 20).map{(i:Int) =>
					Seq[Byte](0x10, 0x31, (0x30 + i - 10).byteValue, 0x00, i.byteValue, 0x00, 0x00, 0x00) 
				}
			val elementsArray:Array[Byte] = elements.flatten.toArray
			
			val len = elementsArray.length + 1;
			
			val source = byteArray2DataInput(
					Array[Byte](len.byteValue, 0x00, 0x00, 0x00) ++: elementsArray :+ 0x00.byteValue
			)
			val expected = (0 until 20).map{i => i.toString -> Right(CborValue(i))}.toMap
			val result = new BsonParser().parse(MapBuilder.apply, source).fold({x => x}, {x => x}, {(s,i) => ((s,i))})
			
			assertResult(expected){result}
		}
	}
}
