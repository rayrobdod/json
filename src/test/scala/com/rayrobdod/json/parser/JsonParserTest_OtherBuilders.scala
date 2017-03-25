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

import org.scalatest.FunSpec;
import com.rayrobdod.json.builder.{Builder, ThrowBuilder}
import com.rayrobdod.json.union.StringOrInt
import com.rayrobdod.json.union.JsonValue
import com.rayrobdod.json.union.ParserRetVal
import com.rayrobdod.json.union.ParserRetVal.{BuilderFailure, Complex}
import com.rayrobdod.json.union.Failures.{ExpectedPrimitive, UnknownKey}

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
			
			object SetBuilder extends Builder[StringOrInt, JsonValue, ExpectedPrimitive.type, Set[String]] {
				override type Middle = Set[String]
				def init:Set[String] = Set.empty
				def apply[Input, PF](folding:Set[String], key:StringOrInt, input:Input, parser:Parser[StringOrInt, JsonValue, PF, Input]):ParserRetVal[Set[String], Nothing, PF, ExpectedPrimitive.type] = {
					val inputVal = parser.parsePrimitive(input)
					val inputStr = inputVal match {case ParserRetVal.Primitive(JsonValue.JsonValueString(s)) => s; case _ => "????????"}
					Complex(folding + inputStr)
				}
				def finalize(x:Middle) = ParserRetVal.Complex(x)
			}
			
			object NameBuilder extends Builder[StringOrInt, JsonValue, util.Either[ExpectedPrimitive.type, UnknownKey.type], Name] {
				override type Middle = Name
				override def init:Name = Name("", "", "")
				override def apply[Input, PF](folding:Name, key:StringOrInt, input:Input, parser:Parser[StringOrInt, JsonValue, PF, Input]):ParserRetVal[Name, Nothing, PF, util.Either[ExpectedPrimitive.type, UnknownKey.type]] = {
					// we only expect strings, so might as well parse the value at the beginning
					parser.parsePrimitive(input).builderFailure.map(util.Left.apply _).flatMap{value:JsonValue =>
						((key, value)) match {
							case ((StringOrInt.Left("given"), JsonValue.JsonValueString(x))) => Complex(folding.copy(given = x))
							case ((StringOrInt.Left("middle"), JsonValue.JsonValueString(x))) => Complex(folding.copy(middle = x))
							case ((StringOrInt.Left("family"), JsonValue.JsonValueString(x))) => Complex(folding.copy(family = x))
							case x => BuilderFailure(util.Right(UnknownKey))
						}
					}.mergeToComplex
				}
				override def finalize(x:Middle) = ParserRetVal.Complex(x)
			}
			
			object PersonBuilder extends Builder[StringOrInt, JsonValue, UnknownKey.type, Person] {
				override type Middle = Person
				def init:Person = Person(Name("", "", ""), "", false, Set.empty)
				def apply[Input, PF](folding:Person, key:StringOrInt, input:Input, parser:Parser[StringOrInt, JsonValue, PF, Input]):ParserRetVal[Person, Nothing, PF, UnknownKey.type] = key match {
					case StringOrInt.Left("name") => Complex(folding.copy(n = parser.parse(NameBuilder, input) match {case ParserRetVal.Complex(x) => x; case _ => new Name("","","")}))
					case StringOrInt.Left("gender") => Complex(folding.copy(gender = parser.parse(new ThrowBuilder, input) match {case ParserRetVal.Primitive(JsonValue.JsonValueString(s)) => s; case _ => "????????"}))
					case StringOrInt.Left("isDead") => Complex(folding.copy(isDead = parser.parse(new ThrowBuilder, input) match {case ParserRetVal.Primitive(JsonValue.JsonValueBoolean(s)) => s; case _ => false}))
					case StringOrInt.Left("interests") => Complex(folding.copy(interests = parser.parse(SetBuilder, input).fold({x => x}, {x => Set.empty}, {x => Set.empty}, {x => Set.empty})))
					case _ => BuilderFailure(UnknownKey)
				}
				override def finalize(x:Middle) = ParserRetVal.Complex(x)
			}
			
			val result:Person = new JsonParser().parse(PersonBuilder, json).fold({x => x}, {x => x}, {x => Person(Name("", "", ""), "", false, Set.empty)}, {x => Person(Name("", "", ""), "", false, Set.empty)})
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
