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
