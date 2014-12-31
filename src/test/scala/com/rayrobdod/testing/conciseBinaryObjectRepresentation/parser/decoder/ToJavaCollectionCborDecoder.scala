/*
	Copyright (c) 2014, Raymond Dodge
	All rights srcerved.
	
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
package com.rayrobdod.conciseBinaryObjectRepresentation.parser.decoders

import com.rayrobdod.conciseBinaryObjectRepresentation.Break
import com.rayrobdod.conciseBinaryObjectRepresentation.encoder.PrimitiveCborEncoderTest.HexArrayStringConverter
import org.scalatest.{FunSpec}
import java.text.ParseException;

/**
 */
class ToJavaCollectionCborDecoderTest extends FunSpec
{
	val dec = new ToJavaCollectionCborDecoder()
	
	describe ("ToJavaCollectionCborDecoderTest.decode") {
		
		it ("value for null") {
			val src = Array(0xf6).map{_.byteValue}
			assertResult(null){ dec.decode(src) }
		}
		it ("value for 'true'") {
			val src = Array(0xf5).map{_.byteValue}
			assertResult(true){ dec.decode(src) }
		}
		it ("value for 'false'") {
			val src = Array(0xf4).map{_.byteValue}
			assertResult(false){ dec.decode(src) }
		}
		it ("value for 'break'") {
			val src = Array(0xff).map{_.byteValue}
			assertResult(Break.instance){ dec.decode(src) }
		}
		
		describe ("numbers") {
			it ("value for zero") {
				val src = Array(0x00).map{_.byteValue}
				assertResult(0){ dec.decode(src) }
			}
			it ("value for small positive integer") {
				val src = Array(0x11).map{_.byteValue}
				assertResult(17){ dec.decode(src) }
			}
			it ("value for small negative integer") {
				val src = Array(0x31).map{_.byteValue}
				assertResult(-18){ dec.decode(src) }
			}
			it ("value for -1") {
				val src = Array(0x20).map{_.byteValue}
				assertResult(-1){ dec.decode(src) }
			}
			it ("value for 23") {
				val src = Array(0x17).map{_.byteValue}
				assertResult(23){ dec.decode(src) }
			}
			it ("value for 24") {
				val src = Array(0x18, 0x18).map{_.byteValue}
				assertResult(24){ dec.decode(src) }
			}
			it ("value for 1000") {
				val src = hexArray"1903e8"
				assertResult(1000){ dec.decode(src) }
			}
			it ("value for 1000000") {
				val src = hexArray"1a000f4240"
				assertResult(1000000){ dec.decode(src) }
			}
			it ("value for 1000000000000") {
				val src = hexArray"1b000000e8d4a51000"
				assertResult(1000000000000l){ dec.decode(src) }
			}
			it ("value for 1.5") {
				val src = Array(0xfa, 0x3f, 0xc0, 0, 0).map{_.byteValue}
				assertResult(1.5){ dec.decode(src) }
			}
			it ("value for -4.1") {
				val src = hexArray"fbc010666666666666"
				assertResult(-4.1){ dec.decode(src) }
			}
		}
		
		describe ("strings") {
			it ("value for nil string") {
				val res = Array(0x60).map{_.byteValue}
				assertResult(""){ dec.decode(res) }
			}
			it ("value for string \"a\"") {
				val res = Array(0x61, 0x61).map{_.byteValue}
				assertResult("a"){ dec.decode(res) }
			}
			it ("value for alphabetString") {
				val res = "abcdefghijklmnopqrstuvwxyz"
				val src = Array(0x78, 26).map{_.byteValue} ++ ('a' to 'z').map{_.byteValue}
				assertResult(res){ dec.decode(src) }
			}
			it ("value for hexString len 0") {
				val res = hexArray""
				val src = hexArray"40"
				assertResult(res){ dec.decode(src) }
			}
			it ("value for hexString len 3") {
				val res = hexArray"010203"
				val src = hexArray"43010203"
				assertResult(res){ dec.decode(src) }
			}
			it ("value for hexString len 400") {
				val res = ((1 to 200) ++ (1 to 200)).map{_.byteValue}.toArray
				val src = hexArray"590190" ++ res
				assertResult(res){ dec.decode(src) }
			}
		}
		
		describe ("array") {
		}
	}
	
}

