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
				expectResult(0)(l.getCount)
			}
			
			it ("one") {
				val l = new GetSize;
				
				l.started()
				l.newKeyValue(0x01,"",EmptyDataInput)
				l.ended()
				expectResult(1)(l.getCount)
			}
			
			it ("many") {
				val l = new GetSize;
				val count = 20;
				
				l.started()
				(0 until count).foreach{(x) =>
					l.newKeyValue(0x01,"",EmptyDataInput)
				}
				l.ended()
				expectResult(count)(l.getCount)
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
				expectResult(0)(l.getCount)
			}
			
			it ("one") {
				val l = new GetSize;
				BSONParser.parse(l, commonObjects.helloWorld)
				expectResult(1)(l.getCount)
			}
			
			it ("many") {
				val l = new GetSize;
				BSONParser.parse(l, commonObjects.countTo20)
				expectResult(20)(l.getCount)
			}
		}
		
		it ("should count only a single layer") {
				val l = new GetSize;
				BSONParser.parse(l, commonObjects.recursion)
				expectResult(1)(l.getCount)
			}
	}
}
