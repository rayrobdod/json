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

import org.scalatest.FunSpec;
import scala.collection.immutable.Seq;
import com.rayrobdod.json.parser.FailureParser
import com.rayrobdod.json.parser.IdentityParser

class BuilderTest extends FunSpec {
	
	describe("Builder.mapKey") {
	}
	describe("Builder.mapValue") {
		it ("passes through a parser's falure") {
			val myValue2 = new Object
			
			assertResult(Left("FailureParser", 0)){
				new SeqBuilder(new PrimitiveSeqBuilder[Object]).mapValue[Object].apply(Nil, "sdfa", myValue2, new FailureParser())
			}
		}
	}
	describe("Builder.flatMapValue") {
		def parseInt(x:String):Either[(String,Int),Int] = {
			try {
				Right(java.lang.Integer.parseInt(x))
			} catch {
				case ex:java.lang.NumberFormatException => Left(("Not an integer", 0))
			}
		}
		
		it ("when success and condition passes, continue being success") {
			assertResult(Right(125 :: Nil)){
				new PrimitiveSeqBuilder[Int]
					.flatMapValue[String](parseInt)
					.apply(Nil, 0, "125", new IdentityParser[String])
			}
		}
		it ("when success but condition fails, become a fail") {
			assertResult(Left(("Not an integer", 0))){
				new PrimitiveSeqBuilder[Int]
					.flatMapValue[String](parseInt)
					.apply(Nil, 0, "abc", new IdentityParser[String])
			}
		}
		it ("when failure, continue being a fail") {
			assertResult(Left(("FailureParser", 0))){
				new PrimitiveSeqBuilder[Int]
					.flatMapValue[String](parseInt)
					.apply(Nil, 0, "abc", new FailureParser)
			}
		}
		it ("nesting") {
			import com.rayrobdod.json.union.{StringOrInt, JsonValue}
			import com.rayrobdod.json.union.JsonValue.JsonValueNumber
			
			assertResult(Right(Seq(Seq(1, 2)))){
				new SeqBuilder[StringOrInt, Int, Seq[Int]](new PrimitiveSeqBuilder[Int])
					.flatMapValue[JsonValue]{v => v match {case JsonValueNumber(x) => Right(x.intValue); case _ => Left("unexpected value", 0)}}
					.apply(Seq.empty, 0, "[1,2]":Iterable[Char], new com.rayrobdod.json.parser.JsonParser)
			}
		}
	}
	
}

