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

import scala.beans.BeanProperty;
import java.text.ParseException;
import scala.collection.immutable.Seq;
import org.scalatest.FunSpec;

class SeqBuilderTest extends FunSpec {
	
	describe("SeqBuilder") {
		it ("inits correctly") {
			assertResult(Nil){new SeqBuilder().init}
		}
		it ("Appends value") {
			val myValue = new Object
			
			assertResult(Seq(myValue)){
				new SeqBuilder().apply(Nil, "sdfa", myValue)
			}
		}
		it ("Appends value 2") {
			val myValue1 = new Object
			val myValue2 = new Object
			
			assertResult(Seq(myValue1, myValue2)){
				new SeqBuilder().apply(Seq(myValue1), "sdfa", myValue2)
			}
		}
		it ("childBuilder returns value from constructor") {
			import BeanBuilderTest.MockBuilder
			
			assertResult(MockBuilder){
				new SeqBuilder(MockBuilder).childBuilder("asdf")
			}
		}
		it ("childBuilder returns default value if no constructor") {
			assert{
				new SeqBuilder().childBuilder("sdafdsfa").isInstanceOf[SeqBuilder[_]]
			}
		}
		it ("resultType returns constructor parameter `clazz`") {
			assertResult(classOf[Seq[_]]){
				new SeqBuilder().resultType
			}
		}
	}
	
	describe("SeqBuilder integration") {
		import com.rayrobdod.json.parser.JsonParser
		import BeanBuilderTest.Person
		
		it ("SeqBuilder + JsonParser + primitive") {
			assertResult(Seq("a", "b", "c")){
				new JsonParser(new SeqBuilder).parse(
					"""["a", "b", "c"]"""
				)
			}
		}
		it ("SeqBuilder + JsonParser + BeanBuilder") {
			assertResult(Seq(Person("Mario", 32),Person("Luigi", 32),Person("Peach", 28))){
				new JsonParser(new SeqBuilder(new BeanBuilder(classOf[Person]))).parse(
					"""[
						{"name":"Mario", "age":32},
						{"name":"Luigi", "age":32},
						{"name":"Peach", "age":28}
					]"""
				)
			}
		}
	}
}

