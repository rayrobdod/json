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
package com.rayrobdod.testing.javaScriptObjectNotation.javaCollection

import com.rayrobdod.javaScriptObjectNotation.javaCollection.{
		JSONObject, JSONObjectIterator, JSONObjectEntry
}
import com.rayrobdod.javaScriptObjectNotation.JSONString
import java.util.Map.Entry;
import org.scalatest.{FunSuite, FunSpec}
import org.scalatest.prop.PropertyChecks
import java.text.ParseException;
import scala.collection.JavaConversions.asScalaIterator

/**
 * @author Raymond Dodge
 * @since 2013 Sept 15
 */
class JSONObjectIteratorTest extends FunSpec
{
	implicit def intToJSONString(x:Int):JSONString = {
		JSONString.generateParsed(x.toString)
	}
	
	describe ("A JSON Object Iterator") {
		describe ("with zero values") {
			def empty = new JSONObjectIterator("{}");
			
			it ("does not have a next") {
				assert( !empty.hasNext() )
			}
			it ("errors when trying to get next") {
				intercept[NoSuchElementException]( empty.next() )
			}
			it ("errors when trying to remove") {
				intercept[IllegalStateException]( empty.remove() )
			}
		}
		
		describe ("with zero values and whitespace") {
			def empty = new JSONObjectIterator("{  }");
			
			it ("does not have a next") {
				assert( !empty.hasNext() )
			}
			it ("errors when trying to get next") {
				intercept[NoSuchElementException]( empty.next() )
			}
			it ("errors when trying to remove") {
				intercept[IllegalStateException]( empty.remove() )
			}
		}
		
		describe ("with a single value") {
			def myThing1 = new JSONObjectIterator("""{"hi":1}""");
			def myThing0 = { val a = myThing1; a.next(); a}
			
			it ("does have a next") {
				assert( myThing1.hasNext() )
			}
			it ("has a next with the correct key") {
				assertResult("hi"){myThing1.next().getKey.toString}
			}
			it ("has a next with the correct value") {
				assertResult(1){myThing1.next().getValue}
			}
			it ("cannot remove before next") {
				intercept[IllegalStateException]( myThing1.remove() )
			}
			
			it ("does not have a second next") {
				assert( !myThing0.hasNext() )
			}
			it ("errors when trying to get second next") {
				intercept[NoSuchElementException]( myThing0.next() )
			}
			ignore ("somethingsomething related to remove...") {
				intercept[IllegalStateException]( myThing0.remove() )
			}
		}
		
		describe ("with many values") {
			
			val size = 20
			def manyValueSeq = {
				val seq = new JSONObject();
				(0 until size).foreach{ x => 
					seq.put(x,x)
				}
				seq
			}
			def iterator = manyValueSeq.entrySet.iterator;
			
			it ("iterates through the things in order (IE, is an order-preserving map)") {
				(iterator zip (0 until size).iterator).foreach({(j:Entry[JSONString, Object], i:Int) =>
					assertResult(i){j.getValue}
					assertResult(i.toString){j.getKey.toString}
				}.tupled)
			}
		}
		
	}
}
