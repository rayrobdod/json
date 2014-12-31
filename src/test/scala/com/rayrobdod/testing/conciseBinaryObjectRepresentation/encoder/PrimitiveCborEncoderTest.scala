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
class PrimitiveCborEncoderTest extends FunSpec
{
	val enc = new PrimitiveCborEncoder
	import PrimitiveCborEncoderTest.HexArrayStringConverter
	
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
			val res = Array(0xf6).map{_.byteValue}
			assertResult(res){ enc.encode(null, enc) }
		}
		it ("value for 'true'") {
			val res = Array(0xf5).map{_.byteValue}
			assertResult(res){ enc.encode(true, enc) }
		}
		it ("value for 'false'") {
			val res = Array(0xf4).map{_.byteValue}
			assertResult(res){ enc.encode(false, enc) }
		}
		it ("value for zero") {
			val res = Array(0x00).map{_.byteValue}
			assertResult(res){ enc.encode(0, enc) }
		}
		it ("value for small positive integer") {
			val res = Array(0x11).map{_.byteValue}
			assertResult(res){ enc.encode(17, enc) }
		}
		it ("value for small negative integer") {
			val res = Array(0x31).map{_.byteValue}
			assertResult(res){ enc.encode(-18, enc) }
		}
		it ("value for -1") {
			val res = Array(0x20).map{_.byteValue}
			assertResult(res){ enc.encode(-1, enc) }
		}
		it ("value for 23") {
			val res = Array(0x17).map{_.byteValue}
			assertResult(res){ enc.encode(23, enc) }
		}
		it ("value for 24") {
			val res = Array(0x18, 0x18).map{_.byteValue}
			assertResult(res){ enc.encode(24, enc) }
		}
		it ("value for 1000") {
			val res = hexArray"1903e8"
			assertResult(res){ enc.encode(1000, enc) }
		}
		it ("value for 1000000") {
			val res = hexArray"1a000f4240"
			assertResult(res){ enc.encode(1000000, enc) }
		}
		it ("value for 1000000000000") {
			val res = hexArray"1b000000e8d4a51000"
			assertResult(res){ enc.encode(1000000000000l, enc) }
		}
		it ("value for 1.5") {
			val res = Array(0xfa, 0x3f, 0xc0, 0, 0).map{_.byteValue}
			assertResult(res){ enc.encode(1.5, enc) }
		}
		it ("value for -4.1") {
			val res = hexArray"fbc010666666666666"
			assertResult(res){ enc.encode(-4.1, enc) }
		}
		it ("error for string") {
			intercept[ClassCastException] { enc.encode("Hello", enc) }
		}
		it ("error for other") {
			intercept[ClassCastException] { enc.encode(this, enc) }
		}
	}
	
}

object PrimitiveCborEncoderTest {
	// String Interpolation
	implicit class HexArrayStringConverter(val sc: StringContext) extends AnyVal {
		def hexArray(args: Any*):Array[Byte] = {
			((sc.parts.head):String).grouped(2).map{x => Integer.parseInt(x, 16)}.map{_.byteValue}.toArray
		}
	}
}

