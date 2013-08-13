package com.rayrobdod.testing.javaScriptObjectNotation.parser.listeners

import com.rayrobdod.javaScriptObjectNotation.parser._
import com.rayrobdod.javaScriptObjectNotation.parser.listeners._
import org.scalatest.{FunSpec}

/**
 * @author Raymond Dodge
 * @version 2013 Jun 22
 */
class GetSizeTest extends FunSpec
{
	describe ("GetSize") {
		describe ("should respond to elemStarted/charRead/elemEnded sequences") {
			it ("zero") {
				val l = new GetSize;
				
				l.started()
				l.ended()
				expectResult(0)(l.getCount)
			}
			
			it ("one") {
				val l = new GetSize;
				
				l.started()
				l.elemStarted(-1,'\0')
				l.charRead(-1,'\0')
				l.elemEnded(-1,'\0')
				l.ended()
				expectResult(1)(l.getCount)
			}
			
			it ("many") {
				val l = new GetSize;
				val count = 20;
				
				l.started()
				(0 until count).foreach{(x) =>
					l.elemStarted(x,x.toChar)
					l.charRead(-1,'\0')
					l.elemEnded(x,x.toChar)
				}
				l.ended()
				expectResult(count)(l.getCount)
			}
		}
		
		it ("should not respond to elemStarted/elemEnded with no charRead") {
				val l = new GetSize;
				
				l.started()
				l.elemStarted(-1,'\0')
				l.elemEnded(-1,'\0')
				l.ended()
				expectResult(0)(l.getCount)
		}
		
		describe ("should error if not first given a started message") {
			it ("ended") {
				val l = new GetSize;
				intercept[IllegalStateException] { l.ended() }
			}
			
			it ("elemStarted") {
				val l = new GetSize;
				intercept[IllegalStateException] { l.elemStarted(-1,'\0') }
			}
			
			it ("elemEnded") {
				val l = new GetSize;
				intercept[IllegalStateException] { l.elemEnded(-1,'\0') }
			}
			
			// ???: others
		}
	}
	
	describe ("JSONParser.parse(GetSize)") {
		describe ("should respond to elemStarted/charRead/elemEnded sequences") {
			it ("zero") {
				val l = new GetSize;
				JSONParser.parse(l, "")
				expectResult(0)(l.getCount)
			}
			
			it ("one") {
				val l = new GetSize;
				JSONParser.parse(l, "[a]")
				expectResult(1)(l.getCount)
			}
			
			it ("many") {
				val l = new GetSize;
				val count = 20;
				JSONParser.parse(l, (1 until count).foldLeft("[0"){_ + "," + _} + "]")
				expectResult(count)(l.getCount)
			}
		}
		
		it ("should not respond to elemStarted/elemEnded with no charRead") {
				val l = new GetSize;
				JSONParser.parse(l, "[]")
				expectResult(0)(l.getCount)
		}
		
		it ("should count only a single layer") {
				val l = new GetSize;
				JSONParser.parse(l, "[[[[[[[[]]]]]]]]")
				expectResult(1)(l.getCount)
		}
	}
}
