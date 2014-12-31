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
package com.rayrobdod.binaryJSON.encoder;

import org.scalatest.{FunSpec}
import java.text.ParseException;

/**
 */
class PrimitiveBsonEncoderTest extends FunSpec
{
	val enc = new PrimitiveBsonEncoder
	val key:Array[Byte] = Array('a', '\0').map{_.byteValue}
	
	describe ("PrimitiveBsonEncoderTest.canEncode") {
		it ("true for null") {
			assert{ enc.canEncode(null) }
		}
		it ("true for integer") {
			assert{ enc.canEncode(42) }
		}
		it ("true for double") {
			assert{ enc.canEncode(1.5) }
		}
		it ("true for 'true'") {
			assert{ enc.canEncode(true) }
		}
		it ("true for 'false'") {
			assert{ enc.canEncode(true) }
		}
		it ("false for string") {
			assert{! enc.canEncode("Hello") }
		}
		it ("false for other") {
			assert{! enc.canEncode(this) }
		}
	}
	
	describe ("PrimitiveBsonEncoderTest.encode") {
		it ("value for null") {
			val res = Array(0x0A, 'a', 0).map{_.byteValue}
			
			assertResult(res){ enc.encode(null, key, enc) }
		}
		it ("mirrors key") {
			val res = Array(0x0A, 'm', 'y', 'k', 'e', 'y', 0).map{_.byteValue}
			val key = Array('m', 'y', 'k', 'e', 'y', 0).map{_.byteValue}
			
			assertResult(res){ enc.encode(null, key, enc) }
		}
		it ("value for integer") {
			val res = Array(0x10, 'a', 0, 42, 0, 0, 0).map{_.byteValue}
			assertResult(res){ enc.encode(42, key, enc) }
		}
		it ("value for double") {
			val res = Array(0x01, 'a', 0, 0, 0, 0, 0, 0, 0, 0xf8, 0x3f).map{_.byteValue}
			assertResult(res){ enc.encode(1.5, key, enc) }
		}
		it ("value for 'true'") {
			val res = Array(0x08, 'a', 0, 1).map{_.byteValue}
			assertResult(res){ enc.encode(true, key, enc) }
		}
		it ("value for 'false'") {
			val res = Array(0x08, 'a', 0, 0).map{_.byteValue}
			assertResult(res){ enc.encode(false, key, enc) }
		}
		it ("error for string") {
			intercept[ClassCastException] { enc.encode("Hello", key, enc) }
		}
		it ("error for other") {
			intercept[ClassCastException] { enc.encode(this, key, enc) }
		}
	}
	
}
