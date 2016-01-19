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

class BuildableBuilderTest extends FunSpec {
	import BuildableBuilderTest.Person;
	private implicit def personClass = classOf[Person]
	
	describe("BuildableBuilder") {
		it ("inits correctly") {
			assertResult(new Person("me", 4)){
					new BuildableBuilder(new Person("me", 4)).init
			}
		}
		it ("") {
			val name = "Anony Mouse"
			assertResult(new Person(name, 0)){
				new BuildableBuilder(new Person("", 0)).addDef("name", {(a,b) => a.copy(name = b.toString)}, BuildableBuilderTest.MockBuilder)
						.apply(new Person("", 0), "name", name)
			}
		}
		it ("Can handle the age bean property") {
			val age = 9001
			assertResult(new Person("", age)){
				new BuildableBuilder(new Person("", 0)).addDef("age", {(a,b) => a.copy(age = b.toString.toInt)}, BuildableBuilderTest.MockBuilder)
						.apply(new Person("", 0), "age", age)
			}
		}
		it ("Throws excpetion on unknown key") {
			val age = "9001"
			intercept[IllegalArgumentException]{
				new BuildableBuilder(new Person("", 0)).apply(new Person("", 0), "asdfjkl;", age)
			}
		}
		it ("ignores unknown key after call to ignoreUnknownKeys") {
			val age = "9001"
			assertResult(new Person("", 0)){
				new BuildableBuilder(new Person("", 0)).ignoreUnknownKeys.apply(new Person("", 0), "asdfjkl;", age)
			}
		}
		it ("childBuilder returns value from constructor") {
			import BuildableBuilderTest.MockBuilder
			
			assertResult(MockBuilder){
				new BuildableBuilder(new Person("", 0)).addDef("key", {(a,b) => a}, MockBuilder).childBuilder("key")
			}
		}
		it ("resultType returns constructor parameter `clazz`") {
			assertResult(classOf[Person]){
				new BuildableBuilder(new Person("", 0)).resultType
			}
		}
	}
	
	describe("BuildableBuilder + JsonParser") {
		import com.rayrobdod.json.parser.JsonParser
		
		it ("works") {
			val builder = new BuildableBuilder(new Person("", 0))
					.addDef("name", {(a,b) => a.copy(name = b.toString)}, BuildableBuilderTest.MockBuilder)
					.addDef("age", {(a,b) => a.copy(age = b.toString.toInt)}, BuildableBuilderTest.MockBuilder)
			
			assertResult(Person("nqpppnl",1)){
				new JsonParser(builder).parse(
					"""{"name":"nqpppnl","age":1}"""
				)
			}
		}
	}
}


object BuildableBuilderTest {
	case class Person(val name:String, val age:Int)
	
	object MockBuilder extends Builder[Nothing] {
		def init:Nothing = {throw new UnsupportedOperationException}
		def apply(folding:Nothing, key:String, value:Any):Nothing = {throw new UnsupportedOperationException}
		def childBuilder(key:String):Nothing = {throw new UnsupportedOperationException}
		def resultType:Class[Nothing] = {throw new UnsupportedOperationException}
	}
}
