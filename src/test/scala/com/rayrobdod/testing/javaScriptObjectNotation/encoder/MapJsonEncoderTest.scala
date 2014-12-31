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

import java.util.Collections.emptyMap
import org.scalatest.{FunSpec}
import java.text.ParseException;

/**
 */
class MapJsonEncoderTest extends FunSpec
{
	val enc = new MapJsonEncoder
	val rec = new PrimitiveJsonEncoder
	
	describe ("MapJsonEncoderTest.canEncode") {
		it ("false for null") {
			assert{! enc.canEncode(null) }
		}
		it ("false for integer") {
			assert{! enc.canEncode(42) }
		}
		it ("true for HashMap") {
			assert{ enc.canEncode(new java.util.HashMap()) }
		}
		it ("true for Collections.emptyMap") {
			assert{ enc.canEncode( emptyMap() ) }
		}
	}
	
	describe ("MapJsonEncoderTest.encode") {
		it ("error for null") {
			intercept[ClassCastException] { enc.encode(null, rec) }
		}
		it ("error for integer") {
			intercept[ClassCastException] { enc.encode(42, rec) }
		}
		it ("value for Empty Map") {
			assertResult("{}"){ enc.encode(emptyMap(), rec) }
		}
		it ("value for Arrays.asList list") {
			val src = new java.util.TreeMap[String, Int]()
			src.put("a", 1);
			src.put("b", 2);
			assertResult("{\"a\":1,\"b\":2}"){ enc.encode(src, rec) }
		}
	}
	
}