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

import java.util.{Arrays, ArrayList}
import java.util.Collections.{emptyList, singletonMap, emptyMap}
import org.scalatest.{FunSpec}
import java.text.ParseException;
import PrimitiveCborEncoderTest.HexArrayStringConverter

/**
 */
class AggregateCborEncoderTest extends FunSpec
{
	val enc = new AggregateCborEncoder(Arrays.asList(
			new PrimitiveCborEncoder,
			new StringCborEncoder,
			new ListCborEncoder,
			new MapCborEncoder
	))
	
	describe ("AggregateCborEncoder(base set).canEncode") {
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
	
	describe ("AggregateCborEncoder.encode") {
		it ("error for this") {
			intercept[ClassCastException] { enc.encode(this, enc) }
		}
		it ("value for Empty List") {
			assertResult(hexArray"80"){ enc.encode(emptyList(), enc) }
		}
		it ("value for Empty Map") {
			assertResult(hexArray"A0"){ enc.encode(emptyMap(), enc) }
		}
		it ("value for Integer") {
			assertResult(hexArray"182A"){ enc.encode(42, enc) }
		}
		it ("value for complex entity") {
			val src = new ArrayList[Object](); // 84
			src.add( singletonMap("A", 65) )   // A161411841
			src.add( singletonMap("B", false)) // A16142F4
			src.add( 6.5:java.lang.Double )    // FA40d00000
			src.add( emptyList )               // 80
			
			assertResult(hexArray"84_A161411841_A16142F4_FA40d00000_80"){ enc.encode(src, enc) }
		}
	}
	
}

