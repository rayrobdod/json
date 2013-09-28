/*
	Copyright (c) 2013, Raymond Dodge
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
package com.rayrobdod.testing.javaScriptObjectNotation.parser.listeners

import com.rayrobdod.javaScriptObjectNotation.parser._
import com.rayrobdod.javaScriptObjectNotation.parser.listeners._
import org.scalatest.{FunSpec}
import java.text.ParseException;
import java.util.Arrays.{asList => JList}

/**
 * @author Raymond Dodge
 * @version 2013 Jun 23
 */
class ToScalaCollectionTest extends FunSpec
{
	object ToIntDecoder extends JSONDecoder[java.lang.Integer] {
		def decode(s:String):java.lang.Integer = s.toInt
	}
	
	describe ("ToScalaCollection") {
	}
	
	describe ("JSONParser.parse(ToScalaCollection)") {
		ignore ("should error if string is empty") {
			val l = new ToScalaCollection(ToIntDecoder)
			intercept[ParseException] { JSONParser.parse(l, "") }
		}
		
		it ("should accept an empty array") {
			val l = new ToScalaCollection(ToIntDecoder)
			JSONParser.parse(l, "[]")
			assert(l.result.isEmpty)
		}
		
		it ("should accept a one-element array") {
			val l = new ToScalaCollection(ToIntDecoder)
			JSONParser.parse(l, "[1]")
			expectResult(Seq(1))(l.resultSeq)
		}
		
		it ("should accept a many-element array") {
			val l = new ToScalaCollection(ToIntDecoder)
			JSONParser.parse(l, "[1,2,3,4,5]")
			expectResult(Seq(1,2,3,4,5))(l.resultSeq)
		}
		
		
		
		it ("should accept an empty object") {
			val l = new ToScalaCollection(ToIntDecoder)
			JSONParser.parse(l, "{}")
			assert(l.result.isEmpty)
		}
		
		it ("should accept a one-element object") {
			val l = new ToScalaCollection(ToIntDecoder)
			JSONParser.parse(l, "{\"\":1}")
			expectResult(Map("" -> 1))(l.resultMap)
		}
		
		it ("should accept a many-element object") {
			val l = new ToScalaCollection(ToIntDecoder)
			JSONParser.parse(l, "{\"1\":1,\"2\":2,\"3\":3}")
			expectResult(Map("1" -> 1,"2" -> 2,"3" -> 3))(l.resultMap)
		}
		
		it ("should accept a mixed ... thing") {
			val l = new ToScalaCollection(ToIntDecoder)
			JSONParser.parse(l, "{\"1\":1,2,\"3\":3}")
			expectResult(Seq(Right("1" -> 1), Left(2), Right("3" -> 3)))(l.result)
		}
	}
}
