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

import org.scalatest.{FunSpec}
import java.text.ParseException;




/**
 */
class StringCborEncoderTest extends FunSpec
{
	val enc = new StringCborEncoder
	import PrimitiveCborEncoderTest.HexArrayStringConverter
	
	describe ("StringCborEncoderTest.canEncode") {
		it ("false for null") {
			assert{! enc.canEncode(null) }
		}
		it ("false for integer") {
			assert{! enc.canEncode(42) }
		}
		it ("true for String") {
			assert{ enc.canEncode("Hello") }
		}
		it ("true for byte[]") {
			assert{ enc.canEncode( hexArray"010203" ) }
		}
	}
	
	describe ("StringCborEncoderTest.encode") {
		it ("error for null") {
			intercept[ClassCastException] { enc.encode(null, enc) }
		}
		it ("error for integer") {
			intercept[ClassCastException] { enc.encode(42, enc) }
		}
		it ("value for nil string") {
			val res = Array(0x60).map{_.byteValue}
			assertResult(res){ enc.encode("", enc) }
		}
		it ("value for string \"a\"") {
			val res = Array(0x61, 0x61).map{_.byteValue}
			assertResult(res){ enc.encode("a", enc) }
		}
		it ("value for \"Hello\"") {
			val res = Array(0x65, 'H', 'e', 'l', 'l', 'o').map{_.byteValue}
			assertResult(res){ enc.encode("Hello", enc) }
		}
		it ("value for alphabetString") {
			val res = Array(0x78, 26).map{_.byteValue} ++ ('a' to 'z').map{_.byteValue}
			assertResult(res){ enc.encode("abcdefghijklmnopqrstuvwxyz", enc) }
		}
		it ("value for hexString len 0") {
			val src = hexArray""
			val res = hexArray"40"
			assertResult(res){ enc.encode(src, enc) }
		}
		it ("value for hexString len 3") {
			val src = hexArray"010203"
			val res = hexArray"43010203"
			assertResult(res){ enc.encode(src, enc) }
		}
		it ("value for hexString len 400") {
			val src = ((1 to 200) ++ (1 to 200)).map{_.byteValue}.toArray
			val res = hexArray"590190" ++ src
			assertResult(res){ enc.encode(src, enc) }
		}
	}
	
}

