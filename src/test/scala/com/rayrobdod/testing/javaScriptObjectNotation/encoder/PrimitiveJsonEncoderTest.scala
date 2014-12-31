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
package com.rayrobdod.javaScriptObjectNotation.encoder;

import org.scalatest.{FunSpec}
import java.text.ParseException;

/**
 */
class PrimitiveJsonEncoderTest extends FunSpec
{
	val enc = new PrimitiveJsonEncoder
	
	describe ("PrimitiveJsonEncoder.canEncode") {
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
	
	describe ("PrimitiveJsonEncoder.encode") {
		it ("value for null") {
			assertResult("null"){ enc.encode(null, enc) }
		}
		it ("value for integer") {
			assertResult("42"){ enc.encode(42, enc) }
		}
		it ("value for double") {
			assertResult("1.5"){ enc.encode(1.5, enc) }
		}
		it ("value for 'true'") {
			assertResult("true"){ enc.encode(true, enc) }
		}
		it ("value for 'false'") {
			assertResult("false"){ enc.encode(false, enc) }
		}
		it ("error for string") {
			intercept[ClassCastException] { enc.encode("Hello", enc) }
		}
		it ("error for other") {
			intercept[ClassCastException] { enc.encode(this, enc) }
		}
	}
	
}
