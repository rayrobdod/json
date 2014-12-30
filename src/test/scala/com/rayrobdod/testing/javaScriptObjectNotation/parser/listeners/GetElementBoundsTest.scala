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
				assertResult(-1)(l.getElementStartIndex)
				assertResult(-1)(l.getKeyValueSplitIndex)
				assertResult(-1)(l.getElementEndIndex)
			}
			
			it ("case: not enough elements") {
				val l = new GetElementBounds(2);
				
				l.started()
				l.elemStarted(5,'a')
				l.elemEnded(6,'a')
				l.ended()
				assertResult(-1)(l.getElementStartIndex)
				assertResult(-1)(l.getKeyValueSplitIndex)
				assertResult(-1)(l.getElementEndIndex)
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
				assertResult(s)(l.getElementStartIndex)
				assertResult(c)(l.getKeyValueSplitIndex)
				assertResult(e)(l.getElementEndIndex)
		}
	}
	
	describe ("JSONParser.parse(GetElementBounds)") {
		describe ("should -1 if values are not found") {
			it ("case: no values in list") {
				val l = new GetElementBounds(0);
				JSONParser.parse(l, "")
				assertResult(-1)(l.getElementStartIndex)
				assertResult(-1)(l.getKeyValueSplitIndex)
				assertResult(-1)(l.getElementEndIndex)
			}
			
			it ("case: not enough elements") {
				val l = new GetElementBounds(2);
				JSONParser.parse(l, "[]")
				assertResult(-1)(l.getElementStartIndex)
				assertResult(-1)(l.getKeyValueSplitIndex)
				assertResult(-1)(l.getElementEndIndex)
			}
		}
		
		it ("should respond to the nth elemStarted value") {
				val l = new GetElementBounds(2);
				JSONParser.parse(l, "[:,:,:,:]")
				assertResult(4)(l.getElementStartIndex)
				assertResult(5)(l.getKeyValueSplitIndex)
				assertResult(6)(l.getElementEndIndex)
		}
	}
}
