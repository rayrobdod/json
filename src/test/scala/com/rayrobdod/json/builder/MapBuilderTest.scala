/*
	Copyright (c) 2015, Raymond Dodge
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
package com.rayrobdod.json.builder;

import scala.beans.BeanProperty;
import java.text.ParseException;
import scala.collection.immutable.Map;
import org.scalatest.FunSpec;
import com.rayrobdod.json.parser.IdentityParser
import com.rayrobdod.json.union.StringOrInt
import com.rayrobdod.json.union.JsonValue

class MapBuilderTest extends FunSpec {
	
	describe("MapBuilder") {
		it ("inits correctly") {
			assertResult(Map.empty){new MapBuilder().init}
		}
		it ("Appends value") {
			val myValue = new Object
			
			assertResult(Map("sdfa" -> myValue)){
				new MapBuilder().apply("sdfa").apply(Map.empty, myValue, new IdentityParser[String,Object])
			}
		}
		it ("Appends value 2") {
			val myValue1 = new Object
			val myValue2 = new Object
			
			assertResult(Map("a" -> myValue1, "b" -> myValue2)){
				new MapBuilder().apply("b").apply(Map("a" -> myValue1), myValue2, new IdentityParser[String,Object])
			}
		}
	}
	
	
	describe("MapBuilder integration") {
		import com.rayrobdod.json.parser.JsonParser
		import BeanBuilderTest.Person
		
		it ("MapBuilder + JsonParser + primitive") {
			assertResult(Map("a" -> 61, "b" -> 62, "c" -> 63).map{x => ((StringOrInt(x._1), JsonValue(x._2)))}){
				new JsonParser().parseComplex(
					new MapBuilder[StringOrInt, JsonValue],
					"""{"a":61, "b":62, "c":63}"""
				)
			}
		}
		it ("MapBuilder + JsonParser + BeanBuilder") {
			assertResult(Map("red" -> Person("Mario", 32),"green" -> Person("Luigi", 32),"pink" -> Person("Peach", 28))){
				new JsonParser().parseComplex(
					new StringOrInt.FromStringKeyBuilder(new MapBuilder[String, JsonValue]({s:String => Option(new BeanBuilder(classOf[Person]))})),
					"""{
						"red":{"name":"Mario", "age":32},
						"green":{"name":"Luigi", "age":32},
						"pink":{"name":"Peach", "age":28}
					}"""
				)
			}
		}
	}
}
