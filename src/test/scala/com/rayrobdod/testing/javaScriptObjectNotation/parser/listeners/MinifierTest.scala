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
import java.io.StringWriter;

/**
 * @author Raymond Dodge
 * @version 2013 Jun 25
 */
class MinifierTest extends FunSpec
{
	def minify(s:String):String = {
		val w = new StringWriter
		val l = new Minifier(w)
		
		JSONParser.parse(l, s)
		w.toString
	}
	
	describe ("Minifier") {}
	
	describe ("JSONParser.parse(Minifier)") {
		it ("should remove whitespace from empty array") {
			assertResult("[]")(minify("  [  ]  "))
		}
		
		it ("should trim whitespace from empty object") {
			assertResult("{:}")(minify("  {  :  }  "))
		}
		
		it ("should remove whitespace from around array values") {
			assertResult("{1,2,3}")(minify("{ 1, 2, 3 }"))
		}
		
		it ("should remove whitespace from around object values") {
			assertResult("""{"1":true,"2":false}""")(
				minify("""{ "1" : true , "2" : false }"""))
		}
		
		it ("should recurse") {
			assertResult("[[1,2],[3,4]]")(
				minify(" [ [ 1 , 2 ] , [ 3 , 4 ] ] "))
		}
		
		// stretch goal
		// when is 'if possible'? all UTF-8? ASCII?
		ignore ("should unescape json string values") {
			assertResult("{\"A\"}")(minify("""{ "\""" + """u0041" }"""))
		}
	}
}
