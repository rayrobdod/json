package com.rayrobdod.testing.javaScriptObjectNotation.parser.listeners

import com.rayrobdod.javaScriptObjectNotation.parser._
import com.rayrobdod.javaScriptObjectNotation.parser.listeners._
import org.scalatest.{FunSpec}
import java.text.ParseException;

/**
 * @author Raymond Dodge
 * @version 2013 Jun 23
 */
class JSONArrayValidatorTest extends FunSpec
{
	describe ("JSONArrayValidator") {
		it ("should error if stream is empty") {
			val l = new JSONArrayValidator;
			
			l.started()
			intercept[ParseException] { l.ended() }
		}
		
		it ("should error if wrong brackets are used (start)") {
			val l = new JSONArrayValidator;
			
			l.started()
			intercept[ParseException] { l.openingBracket(5,'{') }
		}
		
		it ("should error if wrong brackets are used (end)") {
			val l = new JSONArrayValidator;
			
			l.started()
			l.openingBracket(5,'[')
			intercept[ParseException] { l.endingBracket(5,'}') }
		}
		
		it ("should error if no ending bracket") {
			val l = new JSONArrayValidator;
			
			l.started()
			l.openingBracket(5,'[')
			intercept[ParseException] { l.ended() }
		}
		
		it ("should accept an empty array") {
			val l = new JSONArrayValidator;
			
			l.started()
			l.openingBracket(5,'[')
			l.endingBracket(5,']')
			l.ended()
		}
		
		it ("should accept a single-element array") {
			val l = new JSONArrayValidator;
			
			l.started()
			l.openingBracket(5,'[')
			l.elemStarted(5,'[')
			l.charRead(5,'5')
			l.elemEnded(5,']')
			l.endingBracket(5,']')
			l.ended()
		}
		
		it ("should accept a multi-element array") {
			val l = new JSONArrayValidator;
			
			l.started()
			l.openingBracket(5,'[')
			l.elemStarted(5,'[')
			l.charRead(5,'5')
			l.elemEnded(5,']')
			l.elemStarted(5,'[')
			l.charRead(5,'5')
			l.elemEnded(5,']')
			l.elemStarted(5,'[')
			l.charRead(5,'5')
			l.elemEnded(5,']')
			l.endingBracket(5,']')
			l.ended()
		}
		
		it ("should error at a keyValueSeparator") {
			val l = new JSONArrayValidator;
			intercept[ParseException] { l.keyValueSeparation(5,'h') }
		}
	}
	
	describe ("JSONParser.parse(JSONArrayValidator)") {
		it ("should error if stream is empty") {
			val l = new JSONArrayValidator;
			intercept[ParseException] { JSONParser.parse(l, "") }
		}
		
		it ("should error if wrong brackets are used (start)") {
			val l = new JSONArrayValidator;
			intercept[ParseException] { JSONParser.parse(l, "{}") }
		}
		
		it ("should error if wrong brackets are used (end)") {
			val l = new JSONArrayValidator;
			intercept[ParseException] { JSONParser.parse(l, "[}") }
		}
		
		it ("should error if no ending bracket") {
			val l = new JSONArrayValidator;
			intercept[ParseException] { JSONParser.parse(l, "[") }
		}
		
		it ("should accept an empty array") {
			val l = new JSONArrayValidator;
			JSONParser.parse(l, "[]")
		}
		
		it ("should accept a single-element array") {
			val l = new JSONArrayValidator;
			JSONParser.parse(l, "[true]")
		}
		
		it ("should accept a multi-element array") {
			val l = new JSONArrayValidator;
			JSONParser.parse(l, "[1,2,3,4]")
		}
		
		it ("should error at a keyValueSeparator") {
			val l = new JSONArrayValidator;
			intercept[ParseException] { JSONParser.parse(l, "[:]") }
		}
		
		it ("should error at a keyValueSeparator (2)") {
			val l = new JSONArrayValidator;
			intercept[ParseException] { JSONParser.parse(l, "[1,2,3:4,5]") }
		}
		
		it ("should error at an empty value (middle of list)") {
			val l = new JSONArrayValidator;
			intercept[ParseException] { JSONParser.parse(l, "[1,2,,3]") }
		}
		
		it ("should error at an empty value (end of list)") {
			val l = new JSONArrayValidator;
			intercept[ParseException] { JSONParser.parse(l, "[1,2,3,]") }
		}
	}
}
