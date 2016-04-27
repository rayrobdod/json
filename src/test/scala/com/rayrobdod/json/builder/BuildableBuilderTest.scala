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

import java.text.ParseException;
import scala.collection.immutable.Map;
import org.scalatest.FunSpec;
import com.rayrobdod.json.union.JsonValue
import com.rayrobdod.json.union.StringOrInt
import com.rayrobdod.json.parser.IdentityParser
import com.rayrobdod.json.builder.BuildableBuilder.KeyDef

class BuildableBuilderTest extends FunSpec {
	import BuildableBuilderTest.Person;
	private implicit def personClass = classOf[Person]
	
	describe("BuildableBuilder") {
		it ("inits correctly") {
			assertResult(new Person("me", 4)){
					new BuildableBuilder(new Person("me", 4)).init
			}
		}
		it ("Acts upon provided keydef") {
			val name = "Anony Mouse"
			assertResult(new Person(name, 0)){
				new BuildableBuilder(new Person("", 0))
						.addDef("name", new KeyDef[String, String, Person]{def apply[I] = {(s,i,p) => s.copy(name = p.parse(new ThrowBuilder(), i).fold({x => x}, {x => x}))}})
						.apply("name", new Person("", 0), name, new IdentityParser)
			}
		}
		it ("Acts upon provided keydef (2)") {
			val age = 9001
			assertResult(new Person("", age)){
				new BuildableBuilder(new Person("", 0))
						.addDef("age", new KeyDef[String, Int, Person]{def apply[I] = {(s,i,p) => s.copy(age = p.parse(new ThrowBuilder(), i).fold({x => x}, {x => x}))}})
						.apply("age", new Person("", 0), age, new IdentityParser)
			}
		}
		it ("Throws excpetion on unknown key") {
			val age = "9001"
			intercept[IllegalArgumentException]{
				new BuildableBuilder[String, String, Person](new Person("", 0))
						.apply("asdfjkl;", new Person("", 0), "hello", new IdentityParser)
			}
		}
		it ("ignores unknown key after call to ignoreUnknownKeys") {
			val age = "9001"
			assertResult(new Person("", 0)){
				new BuildableBuilder[String, String, Person](new Person("", 0)).ignoreUnknownKeys
						.apply("asdfjkl;", new Person("", 0), "hello", new IdentityParser)
			}
		}
	}
	
	describe("BuildableBuilder + JsonParser") {
		import com.rayrobdod.json.parser.JsonParser
		import com.rayrobdod.json.union.JsonValue._
		
		it ("works") {
			val builder = new BuildableBuilder[StringOrInt, JsonValue, Person](new Person("", 0))
				.addDef("name", new KeyDef[StringOrInt, JsonValue, Person]{ def apply[I] = {(s,i,p) => p.parse(new ThrowBuilder(), i) match {case Right(JsonValueString(i)) => s.copy(name = i); case _ => throw new IllegalArgumentException}}})
				.addDef("age", new KeyDef[StringOrInt, JsonValue, Person]{ def apply[I] = {(s,i,p) => p.parse(new ThrowBuilder(), i) match {case Right(JsonValueNumber(i)) => s.copy(age = i.intValue); case _ => throw new IllegalArgumentException}}})
			
			assertResult(Person("nqpppnl",1)){
				new JsonParser().parse(builder, 
					"""{"name":"nqpppnl","age":1}"""
				).left.get
			}
		}
		it ("nested") {
			val exp = Seq(Person("a", 5), Person("b", 6))
			
			val personBuilder = new BuildableBuilder[StringOrInt, JsonValue, Person](new Person("", 0))
				.addDef("name", new KeyDef[StringOrInt, JsonValue, Person]{ def apply[I] = {(s,i,p) => p.parse(new ThrowBuilder(), i) match {case Right(JsonValueString(i)) => s.copy(name = i); case _ => throw new IllegalArgumentException}}})
				.addDef("age", new KeyDef[StringOrInt, JsonValue, Person]{ def apply[I] = {(s,i,p) => p.parse(new ThrowBuilder(), i) match {case Right(JsonValueNumber(i)) => s.copy(age = i.intValue); case _ => throw new IllegalArgumentException}}})
			
			val seqBuilder = new BuildableBuilder[StringOrInt, JsonValue, Seq[Person]](
				Nil,
				new KeyDef[StringOrInt, JsonValue, Seq[Person]]{ def apply[I] = {(s,i,p) => s :+ p.parse(personBuilder, i).fold({x => x},{x => throw new IllegalArgumentException})}}
			)
				
			assertResult(exp){
				new JsonParser().parse(seqBuilder, 
					"""[{"name":"a","age":5},{"name":"b","age":6}]"""
				).left.get
			}
		}
	}
}


object BuildableBuilderTest {
	case class Person(val name:String, val age:Int)
}
