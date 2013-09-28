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

/**
 * @author Raymond Dodge
 * @version 2013 Jun 23
 */
class JSONObjectValidatorTest extends FunSpec
{
	describe ("JSONObjectValidator") {
		it ("should error if stream is empty") {
			val l = new JSONObjectValidator;
			
			l.started()
			intercept[ParseException] { l.ended() }
		}
		
		it ("should error if wrong brackets are used (start)") {
			val l = new JSONObjectValidator;
			
			l.started()
			intercept[ParseException] { l.openingBracket(5,'[') }
		}
		
		it ("should error if wrong brackets are used (end)") {
			val l = new JSONObjectValidator;
			
			l.started()
			l.openingBracket(5,'{')
			intercept[ParseException] { l.endingBracket(5,']') }
		}
		
		it ("should error if no ending bracket") {
			val l = new JSONObjectValidator;
			
			l.started()
			l.openingBracket(5,'{')
			intercept[ParseException] { l.ended() }
		}
		
		it ("should accept an empty object") {
			val l = new JSONObjectValidator;
			
			l.started()
			l.openingBracket(5,'{')
			l.endingBracket(5,'}')
			l.ended()
		}
		
		it ("should accept a single-element object") {
			val l = new JSONObjectValidator;
			
			l.started()
			l.openingBracket(5,'{')
			l.elemStarted(5,'{')
			l.charRead(5,'"')
			l.charRead(5,'"')
			l.keyValueSeparation(5,':')
			l.charRead(5,'5')
			l.elemEnded(5,'}')
			l.endingBracket(5,'}')
			l.ended()
		}
		
		it ("should accept a multi-element array") {
			val l = new JSONObjectValidator;
			
			l.started()
			l.openingBracket(5,'{')
			('0' to '5').foreach{(x) =>
				l.elemStarted(5,'{')
				l.charRead(5,'"')
				l.charRead(5,x)
				l.charRead(5,'"')
				l.keyValueSeparation(5,':')
				l.charRead(5,'5')
				l.elemEnded(5,'}')
			}
			l.endingBracket(5,'}')
			l.ended()
		}
		
		it ("should error if a later element lacks pairness") {
			val l = new JSONObjectValidator;
			
			l.started()
			l.openingBracket(5,'{')
			('0' to '5').foreach{(x) =>
				l.elemStarted(5,'{')
				l.charRead(5,'"')
				l.charRead(5,x)
				l.charRead(5,'"')
				l.keyValueSeparation(5,':')
				l.charRead(5,'5')
				l.elemEnded(5,'}')
			}
				l.elemStarted(5,'{')
				l.charRead(5,'"')
				l.charRead(5,'6')
				l.charRead(5,'"')
				intercept[IllegalStateException] { l.elemEnded(5,'}') }
		}
		
		it ("should error if no keyValueSeparator") {
			val l = new JSONObjectValidator;
			
			l.started()
			l.openingBracket(5,'{')
			l.elemStarted(5,'{')
			l.charRead(5,'5')
			/* ParseException instead? */
			intercept[IllegalStateException] { l.elemEnded(5,'}') }
		}
		
		it ("should error if multiple keyValueSeparator") {
			val l = new JSONObjectValidator;
			
			l.started()
			l.openingBracket(5,'{')
			l.elemStarted(5,'{')
			l.charRead(5,'5')
			l.keyValueSeparation(5,':')
			/* ParseException instead? */
			intercept[IllegalStateException] { l.keyValueSeparation(5,':') }
		}
	}
	
	describe ("JSONParser.parse(JSONObjectValidator)") {
		it ("should error if stream is empty") {
			val l = new JSONObjectValidator;
			intercept[ParseException] { JSONParser.parse(l, "") }
		}
		
		it ("should error if wrong brackets are used (start)") {
			val l = new JSONObjectValidator;
			intercept[ParseException] { JSONParser.parse(l, "[]") }
		}
		
		it ("should error if wrong brackets are used (end)") {
			val l = new JSONObjectValidator;
			intercept[ParseException] { JSONParser.parse(l, "{]") }
		}
		
		it ("should error if no ending bracket") {
			val l = new JSONObjectValidator;
			intercept[ParseException] { JSONParser.parse(l, "{") }
		}
		
		it ("should accept an empty object") {
			val l = new JSONObjectValidator;
			JSONParser.parse(l, "{}")
		}
		
		it ("should accept a single-element object") {
			val l = new JSONObjectValidator;
			JSONParser.parse(l, "{\"\":true}")
		}
		
		it ("should accept a multi-element array") {
			val l = new JSONObjectValidator;
			JSONParser.parse(l,
				(1 to 5).foldLeft("{\"0\":0"){(x:String, i:Int) =>
					x + ",\"" + i + "\":" + i
				} + "}"
			)
		}
		
		it ("should error if a later element lacks pairness") {
			val l = new JSONObjectValidator;
			intercept[IllegalStateException] { JSONParser.parse(l,
				(1 to 5).foldLeft("{\"0\":0"){(x:String, i:Int) =>
					x + ",\"" + i + "\":" + i
				} + ",true}"
			) }
		}
		
		it ("should error if no keyValueSeparator") {
			val l = new JSONObjectValidator;
			intercept[IllegalStateException] { JSONParser.parse(l, "{1}") }
		}
		
		it ("should error if multiple keyValueSeparator") {
			val l = new JSONObjectValidator;
			intercept[IllegalStateException] { JSONParser.parse(l, "{\"1\":2:3}") }
		}
	}
}
