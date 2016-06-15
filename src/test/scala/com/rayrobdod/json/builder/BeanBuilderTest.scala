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
package com.rayrobdod.json.builder;

import java.text.ParseException;
import scala.beans.BeanProperty;
import scala.collection.immutable.Map;
import scala.util.{Either, Left, Right}
import org.scalatest.FunSpec;
import com.rayrobdod.json.parser.IdentityParser
import com.rayrobdod.json.union.{StringOrInt, JsonValue}

class BeanBuilderTest extends FunSpec {
	import BeanBuilderTest.Person;
	
	describe("BeanBuilder") {
		it ("inits correctly") {
			assertResult(new Person()){
					new BeanBuilder(classOf[Person]).init
			}
		}
		it ("Can handle the name bean property") {
			val name = "Anony Mouse"
			assertResult(Right(new Person(name, 0))){
				new BeanBuilder(classOf[Person]).apply(new Person(), "name", name, new IdentityParser[String,Object])
			}
		}
		it ("Can handle the age bean property") {
			val age = 9001L
			assertResult(Right(new Person("", age))){
				new BeanBuilder(classOf[Person]).apply(new Person(), "age", age, new IdentityParser[String,Any])
			}
		}
		it ("Throws excpetion on incorrect type") {
			val age = "9001"
			assertResult(Left(("com.rayrobdod.json.builder.BeanBuilderTest$Person::setAge with parameter java.lang.String", 0))){
				new BeanBuilder(classOf[Person]).apply(new Person(), "age", age, new IdentityParser[String,Any])
			}
		}
	}
	
	describe("BeanBuilder + JsonParser") {
		import com.rayrobdod.json.parser.JsonParser
		
		it ("works") {
			assertResult(Person("nqpppnl",1)){
				new JsonParser().parse(new BeanBuilder[JsonValue, Person](classOf[Person]).mapKey[StringOrInt]{StringOrInt.unwrapToString},
					"""{"name":"nqpppnl","age":1}"""
				).fold({x => x}, {x => x}, {(s,i) => ((s,i))})
			}
		}
	}
}


object BeanBuilderTest {
	case class Person(
			@BeanProperty var name:String,
			@BeanProperty var age:java.lang.Long
	) {
		def this() = this("", 0)
	}
}
