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

import java.util.Arrays.{asList => List}
import java.util.{ArrayList, Collections}
import org.scalatest.{FunSpec}
import java.text.ParseException;

/**
 */
class ListCborEncoderTest extends FunSpec
{
	val enc = new ListCborEncoder
	val rec = new PrimitiveCborEncoder
	import PrimitiveCborEncoderTest.HexArrayStringConverter
	
	describe ("ListCborEncoderTest.canEncode") {
		it ("false for null") {
			assert{! enc.canEncode(null) }
		}
		it ("false for integer") {
			assert{! enc.canEncode(42) }
		}
		it ("true for ArrayList") {
			assert{ enc.canEncode( new ArrayList() ) }
		}
		it ("true for List") {
			assert{ enc.canEncode( Collections.emptyList ) }
		}
	}
	
	describe ("ListCborEncoderTest.encode") {
		it ("error for null") {
			intercept[ClassCastException] { enc.encode(null, rec) }
		}
		it ("error for integer") {
			intercept[ClassCastException] { enc.encode(42, rec) }
		}
		it ("value for empty list") {
			val res = Array(0x80).map{_.byteValue}
			assertResult(res){ enc.encode(Collections.emptyList, rec) }
		}
		it ("value for List(2) ") {
			val res = Array(0x81, 0x02).map{_.byteValue}
			assertResult(res){ enc.encode( List(2) , rec) }
		}
		it ("length is list size, not byte size") {
			val res = hexArray"82190100f4"
			assertResult(res){ enc.encode( List(256, false) , rec) }
		}
		it ("value for List (length = 30) ") {
			val src = Collections.nCopies(30, 2.byteValue)
			val res = Array(0x98.byteValue, 30.byteValue) ++ Seq.fill[Byte](30){2.byteValue}
			assertResult(res){ enc.encode( src, rec) }
		}
		it ("value for List (length = 400) ") {
			val src = Collections.nCopies(400, 4.byteValue)
			val res = hexArray"990190" ++ Seq.fill[Byte](400){4.byteValue}
			assertResult(res){ enc.encode( src, rec) }
		}
	}
	
}

