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
package com.rayrobdod.testing.binaryJSON.parser.listeners

import com.rayrobdod.binaryJSON.parser._
import com.rayrobdod.binaryJSON.parser.listeners._
import org.scalatest.{FunSpec}

/**
 * @author Raymond Dodge
 * @version 2013 Aug 03
 */
class GetSizeTest extends FunSpec
{
	describe ("GetSize - BSON") {
		describe ("should respond to newKeyValue") {
			it ("zero") {
				val l = new GetSize;
				
				l.started()
				l.ended()
				assertResult(0)(l.getCount)
			}
			
			it ("one") {
				val l = new GetSize;
				
				l.started()
				l.newKeyValue(0x01,"",EmptyDataInput)
				l.ended()
				assertResult(1)(l.getCount)
			}
			
			it ("many") {
				val l = new GetSize;
				val count = 20;
				
				l.started()
				(0 until count).foreach{(x) =>
					l.newKeyValue(0x01,"",EmptyDataInput)
				}
				l.ended()
				assertResult(count)(l.getCount)
			}
		}
		
		describe ("should error if not first given a started message") {
			it ("ended") {
				val l = new GetSize;
				intercept[IllegalStateException] { l.ended() }
			}
			
			it ("newKeyValue") {
				val l = new GetSize;
				intercept[IllegalStateException] {
					l.newKeyValue(0x01,"",EmptyDataInput)
				}
			}
			
			// ???: others
		}
	}
	
	describe ("BSONParser.parse(GetSize)") {
		describe ("should respond to elemStarted/charRead/elemEnded sequences") {
			it ("zero") {
				val l = new GetSize;
				BSONParser.parse(l, commonObjects.empty)
				assertResult(0)(l.getCount)
			}
			
			it ("one") {
				val l = new GetSize;
				BSONParser.parse(l, commonObjects.helloWorld)
				assertResult(1)(l.getCount)
			}
			
			it ("many") {
				val l = new GetSize;
				BSONParser.parse(l, commonObjects.countTo20)
				assertResult(20)(l.getCount)
			}
		}
		
		it ("should count only a single layer") {
				val l = new GetSize;
				BSONParser.parse(l, commonObjects.recursion)
				assertResult(1)(l.getCount)
			}
	}
}
