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

import scala.collection.immutable.Seq;
import org.scalatest.FunSpec;
import com.rayrobdod.json.parser.FailureParser
import com.rayrobdod.json.parser.IdentityParser
import com.rayrobdod.json.parser.SeqParser
import com.rayrobdod.json.parser.PrimitiveSeqParser
import com.rayrobdod.json.parser.CaseClassParser

class SeqBuilderTest extends FunSpec {
	
	describe("PrimitiveSeqBuilder") {
		it ("inits correctly") {
			assertResult(Nil){new PrimitiveSeqBuilder().init}
		}
		it ("Appends value") {
			val myValue = new Object
			
			assertResult(Right(Seq(myValue))){
				new PrimitiveSeqBuilder().apply(Nil, "sdfa", myValue, new IdentityParser[Object])
			}
		}
		it ("Appends value 2") {
			val myValue1 = new Object
			val myValue2 = new Object
			
			assertResult(Right(Seq(myValue1, myValue2))){
				new PrimitiveSeqBuilder().apply(Seq(myValue1), "sdfa", myValue2, new IdentityParser[Object])
			}
		}
		it ("throws when builder gives it a complex value") {
			assertFailure("", 0){
				new PrimitiveSeqBuilder[String].apply(Nil, 5, Seq("a","b","c"), new PrimitiveSeqParser[String])
			}
		}
	}
	describe("SeqBuilder") {
		it ("fails when builder gives it a primitive value") {
			val myValue2 = new Object
			
			assertFailure("",0){
				new SeqBuilder(new PrimitiveSeqBuilder[Object]).apply(Nil, "sdfa", myValue2, new IdentityParser[Object])
			}
		}
		it ("fails when builder gives it a failure") {
			val myValue2 = new Object
			
			assertFailure("",0){
				new SeqBuilder(new PrimitiveSeqBuilder[Object]).apply(Nil, "sdfa", myValue2, new FailureParser)
			}
		}
	}
	
	describe("SeqBuilder integration") {
		import com.rayrobdod.json.union.{JsonValue, StringOrInt}
		import com.rayrobdod.json.parser.JsonParser
		import BeanBuilderTest.Person
		
		it ("PrimitiveSeqBuilder + JsonParser + primitive") {
			assertResult(Seq("a", "b", "c").map{JsonValue(_)}){
				new JsonParser().parse(
					new PrimitiveSeqBuilder[JsonValue].mapKey[StringOrInt]{StringOrInt.unwrapToString},
					"""["a", "b", "c"]"""
				).fold({x => x}, {x => x}, {(s,i) => ((s,i))})
			}
		}
		it ("PrimitiveSeqBuilder + PrimitiveSeqParser") {
			val exp = Seq(15, -4, 2)
			val res = new PrimitiveSeqParser[Int]().parse(new PrimitiveSeqBuilder[Int], exp).fold({x => x}, {x => throw new IllegalArgumentException()}, {(m,i) => throw new IllegalArgumentException()}) 
			assertResult(exp){res}
		}
		it ("SeqBuilder + SeqParser") {
			val exp = Seq(Person("Mario", 32),Person("Luigi", 32),Person("Peach", 28))
			val builder = new SeqBuilder(new CaseClassBuilder[JsonValue, Person](Person("", -1))(classOf[Person])).mapValue[Any]{_ match {case x:Long => JsonValue(x); case x:String => JsonValue(x)}}
			val parser = new SeqParser[String, Any, Person](new CaseClassParser[Person]()(classOf[Person]))(x => x.toString)
			
			assertResult(exp){parser.parse(builder, exp).fold({x => x}, {x => x}, {(s,i) => ((s,i))})}
		}
		it ("SeqBuilder + JsonParser + BeanBuilder") {
			assertResult(Seq(Person("Mario", 32),Person("Luigi", 32),Person("Peach", 28))){
				new JsonParser().parse(
					new SeqBuilder(new BeanBuilder[JsonValue, Person](classOf[Person])).mapKey[StringOrInt]{StringOrInt.unwrapToString},
					"""[
						{"name":"Mario", "age":32},
						{"name":"Luigi", "age":32},
						{"name":"Peach", "age":28}
					]"""
				).fold({x => x}, {x => x}, {(s,i) => ((s,i))})
			}
		}
	}
	
	
	def assertFailure[T](msg:String, idx:Int)(result:Either[_,_]):Unit = result match {
		case Left(x) => {}
		case x => fail("Not a Failure: " + x)
	}
}

