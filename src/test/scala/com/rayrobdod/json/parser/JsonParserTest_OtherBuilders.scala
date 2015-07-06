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
			
			object SetBuilder extends Builder[Set[String]] {
				def init:Set[String] = Set.empty
				def apply(folding:Set[String], key:String, value:Any):Set[String] = {
					folding + value.toString
				}
				def childBuilder(key:String):Builder[Set[String]] = this
				override val resultType:Class[Set[String]] = classOf[Set[String]]
			}
			
			object NameBuilder extends Builder[Name] {
				def init:Name = Name("", "", "")
				def apply(folding:Name, key:String, value:Any):Name = key match {
					case "given" => folding.copy(given = value.toString)
					case "middle" => folding.copy(middle = value.toString)
					case "family" => folding.copy(family = value.toString)
					case _ => throw new ParseException("Unexpected key: " + key, -1)
				}
				def childBuilder(key:String):Builder[_] = SetBuilder
				override val resultType:Class[Name] = classOf[Name]
			}
			
			object PersonBuilder extends Builder[Person] {
				def init:Person = Person(Name("", "", ""), "", false, Set.empty)
				def apply(folding:Person, key:String, value:Any):Person = key match {
					case "name" => folding.copy(n = value.asInstanceOf[Name])
					case "gender" => folding.copy(gender = value.toString)
					case "isDead" => folding.copy(isDead = (value == true))
					case "interests" => folding.copy(interests = value.asInstanceOf[Set[String]])
					case _ => throw new ParseException("Unexpected key: " + key, -1)
				}
				def childBuilder(key:String):Builder[_] = key match {
					case "name" => NameBuilder
					case _ => SetBuilder
				}
				override val resultType:Class[Person] = classOf[Person]
			}
			
			val result:Person = new JsonParser(PersonBuilder).parse(json)
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
