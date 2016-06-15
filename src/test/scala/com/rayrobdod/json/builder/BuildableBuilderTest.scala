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
import scala.util.{Either, Left, Right}
import org.scalatest.FunSpec;
import com.rayrobdod.json.union.JsonValue
import com.rayrobdod.json.union.StringOrInt
import com.rayrobdod.json.parser.{Parser, IdentityParser}
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
			assertResult(Right(new Person(name, 0))){
				new BuildableBuilder(new Person("", 0))
						.addDef("name", new KeyDef[String, String, Person]{def apply[I](s:Person, i:I, p:Parser[String, String, I]) = {Right(s.copy(name = p.parsePrimitive(i).fold({x => ""}, {x => x})))}})
						.apply(new Person("", 0), "name", name, new IdentityParser)
			}
		}
		it ("Acts upon provided keydef (2)") {
			val age = 9001
			assertResult(Right(new Person("", age))){
				new BuildableBuilder(new Person("", 0))
						.addDef("age", new KeyDef[String, Int, Person]{def apply[I](s:Person, i:I, p:Parser[String, Int, I]) = {Right(s.copy(age = p.parsePrimitive(i).fold({x => 0}, {x => x})))}})
						.apply(new Person("", 0), "age", age, new IdentityParser)
			}
		}
		it ("Throws excpetion on unknown key") {
			val age = "9001"
			assertResult(Left("BuildableBuilder has no KeyDef for given key", 0)){
				new BuildableBuilder[String, String, Person](new Person("", 0))
						.apply(new Person("", 0), "asdfjkl;", "hello", new IdentityParser)
			}
		}
		it ("ignores unknown key after call to ignoreUnknownKeys") {
			val age = "9001"
			assertResult(Right(new Person("", 0))){
				new BuildableBuilder[String, String, Person](new Person("", 0)).ignoreUnknownKeys
						.apply(new Person("", 0), "asdfjkl;", "hello", new IdentityParser)
			}
		}
	}
	
	describe("BuildableBuilder + JsonParser") {
		import com.rayrobdod.json.parser.JsonParser
		import com.rayrobdod.json.union.JsonValue._
		
		it ("works") {
			val builder = new BuildableBuilder[StringOrInt, JsonValue, Person](new Person("", 0))
				.addDef("name", new KeyDef[StringOrInt, JsonValue, Person]{ def apply[I](s:Person, i:I, p:Parser[StringOrInt, JsonValue, I]) = {p.parsePrimitive(i).right.flatMap{_ match {case JsonValueString(i) => Right(s.copy(name = i)); case ex => Left("name not string: " + ex, 0)}}}})
				.addDef("age", new KeyDef[StringOrInt, JsonValue, Person]{ def apply[I](s:Person, i:I, p:Parser[StringOrInt, JsonValue, I]) = {p.parsePrimitive(i).right.flatMap{_ match {case JsonValueNumber(i) => Right(s.copy(age = i.intValue)); case ex => Left("age not number: " + ex, 0)}}}})
			
			assertResult(Person("nqpppnl",1)){
				new JsonParser().parse(builder, 
					"""{"name":"nqpppnl","age":1}"""
				).fold({x => x}, {x => x}, {(s,i) => ((s,i))})
			}
		}
		it ("nested") {
			val exp = Seq(Person("a", 5), Person("b", 6))
			
			val personBuilder = new BuildableBuilder[StringOrInt, JsonValue, Person](new Person("", 0))
				.addDef("name", new KeyDef[StringOrInt, JsonValue, Person]{ def apply[I](s:Person, i:I, p:Parser[StringOrInt, JsonValue, I]) = {p.parsePrimitive(i).right.flatMap{_ match {case JsonValueString(i) => Right(s.copy(name = i)); case ex => Left("name not string: " + ex, 0)}}}})
				.addDef("age", new KeyDef[StringOrInt, JsonValue, Person]{ def apply[I](s:Person, i:I, p:Parser[StringOrInt, JsonValue, I]) = {p.parsePrimitive(i).right.flatMap{_ match {case JsonValueNumber(i) => Right(s.copy(age = i.intValue)); case ex => Left("age not number: " + ex, 0)}}}})
			
			val seqBuilder = new BuildableBuilder[StringOrInt, JsonValue, Seq[Person]](
				Nil,
				new KeyDef[StringOrInt, JsonValue, Seq[Person]]{ def apply[I](s:Seq[Person], i:I, p:Parser[StringOrInt, JsonValue, I]) = {p.parse(personBuilder, i).fold({x => Right(x)},{x => Left("ASFD",0)},{(a,b) => Left(a,b)}).right.map{y => s :+ y}}}
			)
				
			assertResult(exp){
				new JsonParser().parse(seqBuilder, 
					"""[{"name":"a","age":5},{"name":"b","age":6}]"""
				).fold({x => x}, {x => x}, {(s,i) => ((s,i))})
			}
		}
	}
}


object BuildableBuilderTest {
	case class Person(val name:String, val age:Int)
}
