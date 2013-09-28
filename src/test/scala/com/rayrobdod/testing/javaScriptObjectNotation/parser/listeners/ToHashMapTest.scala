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
class ToHashMapTest extends FunSpec
{
	object ToIntDecoder extends JSONDecoder[Int] {
		def decode(s:String) = s.toInt
	}
	
	describe ("ToArrayList") {
	}
	
	describe ("JSONParser.parse(ToArrayList)") {
		ignore ("??? if string is empty") {
			val l = new ToHashMap(ToIntDecoder)
			intercept[ParseException] { JSONParser.parse(l, "") }
		}
		
		it ("should error for wrong brackets (start)") {
			val l = new ToHashMap(ToIntDecoder)
			intercept[ParseException] { JSONParser.parse(l, "[") }
		}
		
		it ("should error for wrong brackets (end)") {
			val l = new ToHashMap(ToIntDecoder)
			intercept[ParseException] { JSONParser.parse(l, "{]") }
		}
		
		it ("should accept an empty array") {
			val l = new ToHashMap(ToIntDecoder)
			JSONParser.parse(l, "{}")
			l.getResult.isEmpty
		}
		
		it ("should accept a one-element array") {
			val l = new ToHashMap(ToIntDecoder)
			JSONParser.parse(l, "{\"\":1}")
			expectResult(1)(l.getResult.get(""))
		}
		
		it ("should accept a many-element array") {
			val l = new ToHashMap(ToIntDecoder)
			JSONParser.parse(l, """{"1":1,"2":2,"3":3,"4":4,"5":5}""")
			expectResult(1)(l.getResult.get("1"))
			expectResult(2)(l.getResult.get("2"))
			expectResult(3)(l.getResult.get("3"))
			expectResult(4)(l.getResult.get("4"))
			expectResult(5)(l.getResult.get("5"))
		}
		
	}
}
