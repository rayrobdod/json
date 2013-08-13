package com.rayrobdod.testing.javaScriptObjectNotation.parser.listeners

import com.rayrobdod.javaScriptObjectNotation.parser._
import com.rayrobdod.javaScriptObjectNotation.parser.listeners._
import org.scalatest.{FunSpec}

/**
 * @author Raymond Dodge
 * @version 2013 Jun 23
 */
class GetElementBoundsTest extends FunSpec
{
	describe ("GetElementBounds") {
		describe ("should -1 if values are not found") {
			it ("case: no values in list") {
				val l = new GetElementBounds(0);
				
				l.started()
				l.ended()
				expectResult(-1)(l.getElementStartIndex)
				expectResult(-1)(l.getKeyValueSplitIndex)
				expectResult(-1)(l.getElementEndIndex)
			}
			
			it ("case: not enough elements") {
				val l = new GetElementBounds(2);
				
				l.started()
				l.elemStarted(5,'a')
				l.elemEnded(6,'a')
				l.ended()
				expectResult(-1)(l.getElementStartIndex)
				expectResult(-1)(l.getKeyValueSplitIndex)
				expectResult(-1)(l.getElementEndIndex)
			}
		}
		
		it ("should respond to the nth elemStarted value") {
				val l = new GetElementBounds(2);
				val (s, c, e) = (101,102,103);
				
				l.started()
				l.elemStarted(1,'d')
				l.keyValueSeparation(5,'5')
				l.elemEnded(2,'2')
				l.elemStarted(3,'3')
				l.elemEnded(4,'4')
				l.elemStarted(s,'s')
				l.keyValueSeparation(c,'c')
				l.elemEnded(e,'e')
				l.elemStarted(6,'6')
				l.elemEnded(7,'7')
				l.ended()
				expectResult(s)(l.getElementStartIndex)
				expectResult(c)(l.getKeyValueSplitIndex)
				expectResult(e)(l.getElementEndIndex)
		}
	}
	
	describe ("JSONParser.parse(GetElementBounds)") {
		describe ("should -1 if values are not found") {
			it ("case: no values in list") {
				val l = new GetElementBounds(0);
				JSONParser.parse(l, "")
				expectResult(-1)(l.getElementStartIndex)
				expectResult(-1)(l.getKeyValueSplitIndex)
				expectResult(-1)(l.getElementEndIndex)
			}
			
			it ("case: not enough elements") {
				val l = new GetElementBounds(2);
				JSONParser.parse(l, "[]")
				expectResult(-1)(l.getElementStartIndex)
				expectResult(-1)(l.getKeyValueSplitIndex)
				expectResult(-1)(l.getElementEndIndex)
			}
		}
		
		it ("should respond to the nth elemStarted value") {
				val l = new GetElementBounds(2);
				JSONParser.parse(l, "[:,:,:,:]")
				expectResult(4)(l.getElementStartIndex)
				expectResult(5)(l.getKeyValueSplitIndex)
				expectResult(6)(l.getElementEndIndex)
		}
	}
}
