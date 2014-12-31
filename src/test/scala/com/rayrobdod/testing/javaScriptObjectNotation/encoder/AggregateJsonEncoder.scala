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

import java.util.{Arrays, ArrayList}
import java.util.Collections.{emptyList, singletonMap, emptyMap}
import org.scalatest.{FunSpec}
import java.text.ParseException;

/**
 */
class AggregateJsonEncoderTest extends FunSpec
{
	val enc = new AggregateJsonEncoder(Arrays.asList(
			new PrimitiveJsonEncoder,
			new StringJsonEncoder,
			new ListJsonEncoder,
			new MapJsonEncoder
	))
	
	describe ("AggregateJsonEncoder(base set).canEncode") {
		it ("true for null") {
			assert{ enc.canEncode(null) }
		}
		it ("true for integer") {
			assert{ enc.canEncode(42) }
		}
		it ("true for ArrayList") {
			assert{ enc.canEncode(new ArrayList()) }
		}
		it ("false for this") {
			assert{! enc.canEncode( this ) }
		}
	}
	
	describe ("AggregateJsonEncoder.encode") {
		it ("error for this") {
			intercept[ClassCastException] { enc.encode(this, enc) }
		}
		it ("value for Empty List") {
			assertResult("[]"){ enc.encode(emptyList(), enc) }
		}
		it ("value for Empty Map") {
			assertResult("{}"){ enc.encode(emptyMap(), enc) }
		}
		it ("value for Integer") {
			assertResult("42"){ enc.encode(42, enc) }
		}
		it ("value for complex entity") {
			val src = new ArrayList[Object]();
			src.add( singletonMap("A", 65) )
			src.add( singletonMap("B", false) )
			src.add( 6.5:java.lang.Double )
			src.add( emptyList )
			
			assertResult("""[{"A":65},{"B":false},6.5,[]]"""){ enc.encode(src, enc) }
		}
	}
	
}
