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
import scala.collection.immutable.Map;
import scala.util.{Try, Success, Failure}
import org.scalatest.FunSpec;
import com.rayrobdod.json.parser.IdentityParser
import com.rayrobdod.json.union.JsonValue
import com.rayrobdod.json.union.StringOrInt

class CaseClassBuilderTest extends FunSpec {
	import CaseClassBuilderTest.Person;
	private implicit def personClass = classOf[Person]
	
	describe("CaseClassBuilder") {
		it ("inits correctly") {
			assertResult(new Person("me", 4)){
					new CaseClassBuilder(new Person("me", 4)).init
			}
		}
		it ("Can handle the name bean property") {
			val name = "Anony Mouse"
			assertResult(Success(new Person(name, 0))){
				new CaseClassBuilder(new Person("", 0)).apply(
						new Person("", 0), "name", name, new IdentityParser[String,Object])
			}
		}
		it ("Can handle the age bean property") {
			val age = 9001L
			assertResult(Success(new Person("", age))){
				new CaseClassBuilder(new Person("", 0)).apply(
						new Person("", 0), "age", age, new IdentityParser[String,Any])
			}
		}
		it ("Throws excpetion on incorrect type") {
			val age = "9001"
			assertFailure(classOf[IllegalArgumentException]){
				new CaseClassBuilder(new Person("", 0)).apply(
						new Person("", 0), "age", age, new IdentityParser[String,Object])
			}
		}
		it ("Throws excpetion on unknown key") {
			val age = "9001"
			assertFailure(classOf[IllegalArgumentException]){
				new CaseClassBuilder(new Person("", 0)).apply(
						new Person("", 0), "asdfjkl;", age, new IdentityParser[String,Object])
			}
		}
	}
	
	describe("CaseClassBuilder + JsonParser") {
		import com.rayrobdod.json.parser.JsonParser
		
		it ("works") {
			assertResult(Person("nqpppnl",1)){
				new JsonParser().parse(
					new CaseClassBuilder[JsonValue, Person](new Person("", 0)).mapKey[StringOrInt]{StringOrInt.unwrapToString},
					"""{"name":"nqpppnl","age":1}"""
				).get.left.get
			}
		}
	}
	
	
	
	def assertFailure[T](clazz:Class[T])(result:Try[_]):Unit = result match {
		case Failure(x) => {
			if (! clazz.isInstance(x)) {
				fail("Wrong type of failure: " + x)
			}
		}
		case x => fail("Not a Failure: " + x)
	}
}


object CaseClassBuilderTest {
	case class Person(val name:String, val age:java.lang.Long)
}
