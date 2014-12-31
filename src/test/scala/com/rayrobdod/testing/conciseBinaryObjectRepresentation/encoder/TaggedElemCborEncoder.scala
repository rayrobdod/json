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

import java.net.URI
import java.util.UUID
import org.scalatest.{FunSpec}
import java.text.ParseException;

/**
 */
class TaggedElemCborEncoderTest extends FunSpec
{
	val enc = new TaggedElemCborEncoder
	import PrimitiveCborEncoderTest.HexArrayStringConverter
	
	describe ("TaggedElemCborEncoder.canEncode") {
		it ("false for null") {
			assert{! enc.canEncode(null) }
		}
		it ("false for integer") {
			assert{! enc.canEncode(42) }
		}
		it ("true for URI") {
			assert{ enc.canEncode(new URI("about:blank")) }
		}
		it ("true for UUID") {
			assert{ enc.canEncode(UUID.fromString("58915957-072a-4758-8d31-5eb29ce15643"))}
		}
	}
	
	describe ("TaggedElemCborEncoder.encode") {
		it ("error for null") {
			intercept[ClassCastException] { enc.encode(null, enc) }
		}
		it ("error for integer") {
			intercept[ClassCastException] { enc.encode(42, enc) }
		}
		it ("value for URI(about:blank)") {
			val res = hexArray"d8206B" ++ "about:blank".map{_.byteValue}
			assertResult(res){ enc.encode(new URI("about:blank"), enc) }
		}
		it ("value for UUID") {
			val src = UUID.fromString("58915957-072a-4758-8d31-5eb29ce15643")
			val res = hexArray"d82550_58915957072a47588d315eb29ce15643"
			assertResult(res){ enc.encode(src, enc) }
		}
	}
	
}

