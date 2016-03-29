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
package com.rayrobdod.json.parser;

import java.text.ParseException;
import scala.collection.immutable.Map;
import org.scalatest.FunSpec;
import com.rayrobdod.json.builder.Builder;
import com.rayrobdod.json.union.StringOrInt
import com.rayrobdod.json.union.JsonValue

class JsonParserTest_OtherBuilders extends FunSpec {
	describe("JsonParser with other builders") {
		it ("works") {
		
			case class Name(given:String, middle:String, family:String)
			case class Person(n:Name, gender:String, isDead:Boolean, interests:Set[String])
			
			val json = """{
					"name":{
						"given":"Raymond",
						"middle":"Robert",
						"family":"Dodge"
					},
					"gender":"male",
					"isDead":false,
					"interests":["bowling", "tennis", "programming", "twitch plays pokémon"]
			}"""
			
			object SetBuilder extends Builder[StringOrInt, JsonValue, Set[String]] {
				def init:Set[String] = Set.empty
				def apply[Input](key:StringOrInt):Function3[Set[String], Input, Parser[StringOrInt, JsonValue, Input], Set[String]] = {(folding, input, parser) =>
					val inputVal = parser.parsePrimitive(input)
					val inputStr = inputVal match {case JsonValue.JsonValueString(s) => s; case _ => "????????"}
					folding + inputStr
				}
			}
			
			object NameBuilder extends Builder[StringOrInt,JsonValue,Name] {
				def init:Name = Name("", "", "")
				def apply[Input](key:StringOrInt):Function3[Name, Input, Parser[StringOrInt, JsonValue, Input], Name] = {(folding, input, parser) =>
					val value = parser.parsePrimitive(input) match {case JsonValue.JsonValueString(s) => s; case _ => "????????"}
					
					key match {
						case StringOrInt.Left("given") => folding.copy(given = value)
						case StringOrInt.Left("middle") => folding.copy(middle = value)
						case StringOrInt.Left("family") => folding.copy(family = value)
						case _ => throw new ParseException("Unexpected key: " + key, -1)
					}
				}
			}
			
			object PersonBuilder extends Builder[StringOrInt, JsonValue, Person] {
				def init:Person = Person(Name("", "", ""), "", false, Set.empty)
				def apply[Input](key:StringOrInt):Function3[Person, Input, Parser[StringOrInt, JsonValue, Input], Person] = key match {
					case StringOrInt.Left("name") => {(folding, input, parser) => folding.copy(n = parser.parseComplex(NameBuilder, input))}
					case StringOrInt.Left("gender") => {(folding, input, parser) => folding.copy(gender = parser.parsePrimitive(input) match {case JsonValue.JsonValueString(s) => s; case _ => "????????"})}
					case StringOrInt.Left("isDead") => {(folding, input, parser) => folding.copy(isDead = parser.parsePrimitive(input) match {case JsonValue.JsonValueBoolean(s) => s; case _ => false})}
					case StringOrInt.Left("interests") => {(folding, input, parser) => folding.copy(interests = parser.parseComplex(SetBuilder, input))}
					case _ => throw new ParseException("Unexpected key: " + key, -1)
				}
			}
			
			val result:Person = new JsonParser().parseComplex(PersonBuilder, json)
			val expected = Person(
				Name( "Raymond", "Robert", "Dodge"),
				"male",
				false,
				Set("bowling", "tennis", "programming", "twitch plays pokémon")
			)
			assertResult(expected){result}
		}
	}
}
