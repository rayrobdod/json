/*
	Copyright (c) 2015-2016, Raymond Dodge
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

import scala.util.{Left, Right}
import org.scalatest.FunSpec;
import com.rayrobdod.json.builder.{Builder, ThrowBuilder}
import com.rayrobdod.json.union.StringOrInt
import com.rayrobdod.json.union.JsonValue
import com.rayrobdod.json.union.ParserRetVal

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
				def apply[Input](folding:Set[String], key:StringOrInt, input:Input, parser:Parser[StringOrInt, JsonValue, Input]):Either[(String, Int), Set[String]] = {
					val inputVal = parser.parse(new ThrowBuilder, input)
					val inputStr = inputVal match {case ParserRetVal.Primitive(JsonValue.JsonValueString(s)) => s; case _ => "????????"}
					Right(folding + inputStr)
				}
			}
			
			object NameBuilder extends Builder[StringOrInt, JsonValue, Name] {
				override def init:Name = Name("", "", "")
				override def apply[Input](folding:Name, key:StringOrInt, input:Input, parser:Parser[StringOrInt, JsonValue, Input]):Either[(String, Int), Name] = {
					// we only expect strings, so might as well parse the value at the beginning
					parser.parsePrimitive(input).right.flatMap{value:JsonValue =>
						((key, value)) match {
							case ((StringOrInt.Left("given"), JsonValue.JsonValueString(x))) => Right(folding.copy(given = x))
							case ((StringOrInt.Left("middle"), JsonValue.JsonValueString(x))) => Right(folding.copy(middle = x))
							case ((StringOrInt.Left("family"), JsonValue.JsonValueString(x))) => Right(folding.copy(family = x))
							case x => Left("NameBuilder: Unexpected key/value: " + x, 0)
						}
					}
				}
			}
			
			object PersonBuilder extends Builder[StringOrInt, JsonValue, Person] {
				def init:Person = Person(Name("", "", ""), "", false, Set.empty)
				def apply[Input](folding:Person, key:StringOrInt, input:Input, parser:Parser[StringOrInt, JsonValue, Input]):Either[(String, Int), Person] = key match {
					case StringOrInt.Left("name") => Right(folding.copy(n = parser.parse(NameBuilder, input) match {case ParserRetVal.Complex(x) => x; case _ => new Name("","","")}))
					case StringOrInt.Left("gender") => Right(folding.copy(gender = parser.parse(new ThrowBuilder, input) match {case ParserRetVal.Primitive(JsonValue.JsonValueString(s)) => s; case _ => "????????"}))
					case StringOrInt.Left("isDead") => Right(folding.copy(isDead = parser.parse(new ThrowBuilder, input) match {case ParserRetVal.Primitive(JsonValue.JsonValueBoolean(s)) => s; case _ => false}))
					case StringOrInt.Left("interests") => Right(folding.copy(interests = parser.parse(SetBuilder, input).fold({x => x}, {x => Set.empty}, {(a,b) => Set.empty})))
					case _ => Left("Unexpected key: " + key, 0)
				}
			}
			
			val result:Person = new JsonParser().parse(PersonBuilder, json).fold({x => x}, {x => x}, {(s,i) => Person(Name("", "", ""), "", false, Set.empty)})
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
