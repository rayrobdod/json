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
package com.rayrobdod.testing.javaScriptObjectNotation

import com.rayrobdod.javaScriptObjectNotation.JSONString
import org.scalatest.{FunSuite, FunSpec}
import org.scalatest.prop.PropertyChecks
import java.text.ParseException;
import JSONString.{generateUnparsed, generateParsed}

/**
 * @author Raymond Dodge
 * @version 2013 Jun 22
 */
class JSONStringTest extends FunSpec
{
	
	describe ("A JSON String") {
		describe ("Unparsed constructor") {
			it ("Should reject strings not surrounded by '\"'s"){
				val thingToTest = ""
				assert(! JSONString.isValid(thingToTest))
				intercept[ParseException] {
					generateUnparsed(thingToTest)
				}
			}
			
			it ("Should reject strings containing only one '\"'"){
				val thingToTest = "\""
				assert(! JSONString.isValid(thingToTest))
				intercept[ParseException] {
					generateUnparsed(thingToTest)
				}
			}
			
			it ("Should escape escapable characters"){
				val thingToTest = """"\\\/\"\b\f\n\r\t" """
				assert(JSONString.isValid(thingToTest))
				assert(generateUnparsed(thingToTest
						).toString === "\\/\"\b\f\n\r\t")
			}
			
			describe ("Sould treat unicode escapes") {
				it ("Should correctly respond to a valid escape"){
					val thingToTest = """"\u0041" """
					assert(JSONString.isValid(thingToTest))
					assert(generateUnparsed(thingToTest
							).toString === "\u0041")
				}
				
				it ("Should error if invalid (too short)"){
					val thingToTest = "\"\\u\""
					assert(! JSONString.isValid(thingToTest))
					intercept[ParseException] {
						generateUnparsed(thingToTest)
					}
				}
					
				it ("Should error if invalid (not hex)"){
					val thingToTest = "\"\\uGHDF\""
					assert(! JSONString.isValid(thingToTest))
					intercept[ParseException] {
						generateUnparsed(thingToTest)
					}
				}
			}
			
			it ("Should not allow '\\'s elsewhere in the string"){
				val chars = ('a' to 'z') diff Seq('b','f','n','u','r','t','\\','/','"')
				
				chars.foreach {(c:Char) =>
					intercept[ParseException] {
						generateUnparsed("\"\\" + c + "\"")
					}
				}
			}
			
			it ("Should not allow control chars in the string"){
				val chars = ('\0' until ' ')
				
				chars.foreach {(c:Char) =>
					intercept[ParseException] {
						generateUnparsed("\"" + c + "\"")
					}
				}
			}
		}
		
		describe ("Parsed constructor") {
			it ("should have a toString equal to the parameter"){
				//forAll{(s:String) =>
				//	s should be === generateUnparsed(s).toString
				//}
				
				assert("" === generateParsed("").toString)
				assert("abc" === generateParsed("abc").toString)
				assert("\t" === generateParsed("\t").toString)
				assert("\u1234" === generateParsed("\u1234").toString)
			}
		}
		
		describe ("when empty") {
			val empty = generateUnparsed(""" "" """)
			
			it ("should have a toString to an empty string") {
				assert("" === empty.toString)
			}
			
			it ("should have a length of 0") {
				assert(0 === empty.length)
			}
			
			it ("should throw an IndexOutOfBoundsException for getting its first char") {
				intercept[IndexOutOfBoundsException] { empty.charAt(0) }
			}
			
			it ("should be equal to itself") {
				assert(empty === empty)
			}
			
			it ("should compare equal to itself") {
				assert(empty.compareTo(empty) === 0)
			}
			
			it ("should compare lexographically before everything else") {
				assert(empty.compareTo(generateParsed("any")) < 0)
				assert(generateParsed("any").compareTo(empty) > 0)
			}
			
			it ("should have an empty iterator") {
				assert(! empty.iterator.hasNext())
			}
		}
		
		describe ("for string `\"Hello World\"`") {
			val tested = generateUnparsed(""" "Hello World" """)
			
			it ("should have a length of 11") {
				assert(11 === tested.length)
			}
			
			it ("should say its second char is 'e'") {
				assert('e' === tested.charAt(1))
			}
			
			it ("should throw an IndexOutOfBoundsException for getting its 20th char") {
				intercept[IndexOutOfBoundsException] { tested.charAt(19) }
			}
			
			it ("should compare equal to itself") {
				assert(tested.compareTo(tested) === 0)
			}
			
			it ("should toString to `Hello World`") {
				assert("Hello World" == tested.toString)
			}
			
			it ("should have a working iterator") {
				val it = tested.iterator
				
				assert( it.hasNext )
				assert('H' === it.next)
				assert( it.hasNext )
				assert('e' === it.next)
				assert( it.hasNext )
				assert('l' === it.next)
				assert( it.hasNext )
				assert('l' === it.next)
				assert( it.hasNext )
				assert('o' === it.next)
				assert( it.hasNext )
				assert(' ' === it.next)
				assert( it.hasNext )
				assert('W' === it.next)
				assert( it.hasNext )
				assert('o' === it.next)
				assert( it.hasNext )
				assert('r' === it.next)
				assert( it.hasNext )
				assert('l' === it.next)
				assert( it.hasNext )
				assert('d' === it.next)
				assert(! it.hasNext )
				intercept[NoSuchElementException] { it.next }
			}
		}
		
		describe ("a string consisiting of a single unicode control char") {
			val char = '\u0001'
			val tested = generateParsed("" + char)
			
			it ("should have a length of 1") {
				assert(1 === tested.length)
			}
			
			it ("should say its first char is '\\u0001'") {
				assert(char === tested.charAt(0))
			}
			
			it ("should compare equal to itself") {
				assert(tested.compareTo(tested) === 0)
			}
			
			it ("should unparse to a unicode-escape") {
				assert("\"\\u0001\"" === tested.getUnparsedString())
			}
			it ("should have a working iterator") {
				val it = tested.iterator
				
				assert( it.hasNext )
				assert(char === it.next)
				assert(! it.hasNext )
				intercept[NoSuchElementException] { it.next }
			}
		}
		
		describe ("two differently encoded but equivalent strings (a)") {
			val first = generateUnparsed(""" "a" """)
			val second = generateUnparsed(""" "\""" + """u0061" """)
			
			it ("should be equal") {
				assert(first === second)
			}
			it ("should compare equal") {
				assert(0 === (first compareTo second))
			}
			it ("should both toString to the same string (first)") {
				assert("a" === first.toString)
			}
			it ("should both toString to the same string (second)") {
				assert("a" === second.toString)
			}
		}
	
		describe ("two differently encoded but equivalent strings (\\t)") {
			val first = generateUnparsed(""" "\t" """)
			val second = generateUnparsed(""" "\u0009" """)
			
			it ("should be equal") {
				assert(first === second)
			}
			it ("should compare equal") {
				assert(0 === (first compareTo second))
			}
			it ("should both toString to the same string (first)") {
				assert("\t" === first.toString)
			}
			it ("should both toString to the same string (second)") {
				assert("\t" === second.toString)
			}
		}
	}
}
