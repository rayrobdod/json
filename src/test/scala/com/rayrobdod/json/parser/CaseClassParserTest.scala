/*
	Copyright (c) 2015, Raymond Dodge
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
package com.rayrobdod.json.parser

import org.scalatest.FunSpec
import java.text.ParseException
import scala.collection.immutable.Map
import com.rayrobdod.json.builder.{MapBuilder, CaseClassBuilder, MinifiedJsonObjectBuilder}

class CaseClassParserTest extends FunSpec {
	private implicit def fooClass = classOf[Foo]
	private case class Foo(hello:Long, world:String, bazz:Boolean)
	
	describe("CaseClassParser") {
		it ("""recreates an arbitrary case class""") {
			val exp = Map("hello" -> 43L, "world" -> "world", "bazz" -> true)
			val src = Foo(43L, "world", true)
			val res = new CaseClassParser(new MapBuilder()).parse(src)
			
			assertResult(exp){res}
		}
	}
	describe("CaseClassParser + Json") {
		it ("""can be used with the json stuff to serialze and deserialize a map""") {
			val src = Foo(-5, "asdf", true)
			val json = new CaseClassParser(new MinifiedJsonObjectBuilder()).parse(src)
			val res = new JsonParser(new CaseClassBuilder(Foo(0,"",false))).parse(json)
			
			assertResult(src){res}
		}
	}
}