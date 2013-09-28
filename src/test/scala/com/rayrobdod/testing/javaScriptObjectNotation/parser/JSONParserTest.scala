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
package com.rayrobdod.testing.javaScriptObjectNotation.parser

import com.rayrobdod.javaScriptObjectNotation.parser._
import org.scalatest.{FunSuite, FunSpec}
import org.scalatest.prop.PropertyChecks
import java.text.ParseException;

/**
 * @author Raymond Dodge
 * @version 2013 Jun 22
 */
class JSONParserTest extends FunSpec
{
	class EventToNumberListener extends JSONParseListener {
		private val builder = new StringBuilder
		def result = builder.toString
		
		override def charRead(index:Int, c:Char) {builder += '0'}
		override def elemStarted(index:Int, c:Char) {builder += '1'}
		override def elemEnded(index:Int, c:Char) {builder += '2'}
		override def keyValueSeparation(index:Int, c:Char) {builder += '3'}
		override def openingBracket(index:Int, c:Char) {builder += '4'}
		override def endingBracket(index:Int, c:Char) {builder += '5'}
		override def started {builder += '6'}
		override def ended {builder += '7'}
		override def abort():Boolean = false
	}
	
	class GetCharParameterListener extends JSONParseListener {
		private val builder = new StringBuilder
		def result = builder.toString
		
		override def charRead(index:Int, c:Char) {builder += c}
		override def elemStarted(index:Int, c:Char) {builder += c}
		override def elemEnded(index:Int, c:Char) {builder += c}
		override def keyValueSeparation(index:Int, c:Char) {builder += c}
		override def openingBracket(index:Int, c:Char) {builder += c}
		override def endingBracket(index:Int, c:Char) {builder += c}
		override def started {}
		override def ended {}
		override def abort():Boolean = false
	}
	
	describe ("The JSON Parser") {
		describe ("in regards to order of events") {
			it ("always puts out a start and end event") {
				val l = new EventToNumberListener()
				JSONParser.parse(l, "")
				expectResult("67")(l.result)
			}
			
			it ("should respond to array brackets") {
				val l = new EventToNumberListener()
				JSONParser.parse(l, "[]")
				expectResult("641257")(l.result)
			}
			
			it ("should respond to object brackets") {
				val l = new EventToNumberListener()
				JSONParser.parse(l, "{}")
				expectResult("641257")(l.result)
			}
			
			it ("should respond to a comma inside brackets") {
				val l = new EventToNumberListener()
				JSONParser.parse(l, "{,}")
				expectResult("64121257")(l.result)
			}
			
			it ("should respond to a colon inside brackets") {
				val l = new EventToNumberListener()
				JSONParser.parse(l, "{:}")
				expectResult("6413257")(l.result)
			}
			
			it ("should respond to characters inside brackets") {
				val l = new EventToNumberListener()
				JSONParser.parse(l, "{asb}")
				expectResult("641000257")(l.result)
			}
			
			it ("should use charRead for nested stuff") {
				val l = new EventToNumberListener()
				JSONParser.parse(l, "{{a}}")
				expectResult("641000257")(l.result)
			}
			
			it ("should stop after exiting the outermost layer") {
				val l = new EventToNumberListener()
				JSONParser.parse(l, "{[}]abc")
				expectResult("64100257")(l.result)
			}
			
			// Maybe error instead?
			it ("should not mention a closing bracket if there isn't one") {
				val l = new EventToNumberListener()
				JSONParser.parse(l, "{")
				expectResult("6417")(l.result)
			}
			
			// Maybe error instead?
			it ("should not mention a opening bracket if there isn't one") {
				val l = new EventToNumberListener()
				JSONParser.parse(l, "}")
				expectResult("67")(l.result)
				// INSTEAD(?): expectResult("6257")(l.result)
			}
			
			it ("should not respond to alphanums outside brackets") {
				val l = new EventToNumberListener()
				JSONParser.parse(l, "K9")
				expectResult("67")(l.result)
				// INSTEAD(?): expectResult("6007")(l.result)
			}
			
			it ("multiple colons") {
				val l = new EventToNumberListener()
				JSONParser.parse(l, "{::}")
				expectResult("64133257")(l.result)
			}
		}
		
		describe ("in regards to event parameters") {
			
			it ("should respond to array brackets") {
				val l = new GetCharParameterListener()
				JSONParser.parse(l, "[]")
				expectResult("[[]]")(l.result)
			}
			
			it ("should respond to object brackets") {
				val l = new GetCharParameterListener()
				JSONParser.parse(l, "{}")
				expectResult("{{}}")(l.result)
			}
			
			it ("should respond to commas inside brackets") {
				val l = new GetCharParameterListener()
				JSONParser.parse(l, "{,}")
				expectResult("{{,,}}")(l.result)
			}
			
			it ("should respond to colons inside brackets") {
				val l = new GetCharParameterListener()
				JSONParser.parse(l, "[:]")
				expectResult("[[:]]")(l.result)
			}
			
			it ("should respond to alphanums inside brackets") {
				val l = new GetCharParameterListener()
				JSONParser.parse(l, "[abc]")
				expectResult("[[abc]]")(l.result)
			}
		}
	}
}
