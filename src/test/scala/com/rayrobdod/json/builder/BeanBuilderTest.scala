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

import scala.reflect.BeanProperty;
import java.text.ParseException;
import scala.collection.immutable.Map;
import org.scalatest.FunSpec;

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
			assertResult(new Person(name, 0)){
				new BeanBuilder(classOf[Person]).apply(new Person(), "name", name)
			}
		}
		it ("Can handle the age bean property") {
			val age = 9001l
			assertResult(new Person("", age)){
				new BeanBuilder(classOf[Person]).apply(new Person(), "age", age)
			}
		}
		it ("Throws excpetion on incorrect type") {
			val age = "9001"
			intercept[NoSuchMethodException]{
				new BeanBuilder(classOf[Person]).apply(new Person(), "age", age)
			}
		}
		it ("childBuilder returns value from constructor") {
			import BeanBuilderTest.MockBuilder
			
			assertResult(MockBuilder){
				new BeanBuilder(classOf[Person], Map("key" -> MockBuilder)).childBuilder("key")
			}
		}
		it ("resultType returns constructor parameter `clazz`") {
			assertResult(classOf[Person]){
				new BeanBuilder(classOf[Person]).resultType
			}
		}
	}
	
	describe("BeanBuilder + JsonParser") {
		import com.rayrobdod.json.parser.JsonParser
		
		it ("works") {
			assertResult(Person("nqpppnl",1)){
				new JsonParser(new BeanBuilder(classOf[Person])).parse(
					"""{"name":"nqpppnl","age":1}"""
				)
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
	
	object MockBuilder extends Builder[Nothing] {
		def init:Nothing = {throw new UnsupportedOperationException}
		def apply(folding:Nothing, key:String, value:Any) = {throw new UnsupportedOperationException}
		def childBuilder(key:String) = {throw new UnsupportedOperationException}
		def resultType:Class[Nothing] = {throw new UnsupportedOperationException}
	}
}
