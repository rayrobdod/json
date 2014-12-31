/*
	Copyright (c) 2014, Raymond Dodge
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
package com.rayrobdod.conciseBinaryObjectRepresentation.encoder;

import java.util.{TreeMap, Collections}
import org.scalatest.{FunSpec}
import java.text.ParseException;

/**
 */
class MapCborEncoderTest extends FunSpec
{
	val enc = new MapCborEncoder
	val rec = new PrimitiveCborEncoder
	import PrimitiveCborEncoderTest.HexArrayStringConverter
	
	describe ("ListCborEncoderTest.canEncode") {
		it ("false for null") {
			assert{! enc.canEncode(null) }
		}
		it ("false for integer") {
			assert{! enc.canEncode(42) }
		}
		it ("true for TreeMap") {
			assert{ enc.canEncode( new TreeMap() ) }
		}
		it ("true for Map") {
			assert{ enc.canEncode( Collections.emptyMap ) }
		}
	}
	
	describe ("ListCborEncoderTest.encode") {
		it ("error for null") {
			intercept[ClassCastException] { enc.encode(null, rec) }
		}
		it ("error for integer") {
			intercept[ClassCastException] { enc.encode(42, rec) }
		}
		it ("value for empty map") {
			val res = Array(0xA0).map{_.byteValue}
			assertResult(res){ enc.encode(Collections.emptyMap, rec) }
		}
		it ("value for Map(1 -> 2) ") {
			val res = Array(0xA1, 0x01, 0x02).map{_.byteValue}
			assertResult(res){ enc.encode( Collections.singletonMap(1, 2) , rec) }
		}
	}
	
}

