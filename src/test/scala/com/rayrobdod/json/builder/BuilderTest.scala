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
import com.rayrobdod.json.parser.Parser
import com.rayrobdod.json.parser.FailureParser
import com.rayrobdod.json.parser.IdentityParser

class BuilderTest extends FunSpec {
	
	private[this] final class ReportKeyValueBuilder[A,B] extends Builder[A,B,(A,B)] {
		def init = null
		def apply[Input](folding:(A,B), key:A, input:Input, parser:Parser[A, B, Input]):Either[(String, Int), (A,B)] = {
			parser.parsePrimitive(input).right.map{value => ((key, value))}
		}
	}
	
	
	describe("Builder.mapKey") {
		it ("The new builder's init is the same as the old builder's init") {
			val builder = new ThrowBuilder[Int, String]
			assertResult(builder.init){builder.mapKey[Int]{x => x}.init}
		}
		it ("when a success, continue being a success") {
			assertResult(Right("key", "value")){
				new ReportKeyValueBuilder[String, String]
						.mapKey[Int]{case x => "key"}
						.apply(null, 1, "value",  new IdentityParser[String])
			}
		}
		it ("passes through a parser's failure") {
			assertResult(Left("FailureParser", 0)){
				new ReportKeyValueBuilder[String, String]
						.mapKey[Int]{case x => "key"}
						.apply(null, 1, "value",  new FailureParser)
			}
		}
		it ("passes through a builder's failure") {
			assertResult(Left("using ThrowBuilder::apply", 0)){
				new ThrowBuilder[String, String]
						.mapKey[Int]{case x => "key"}
						.apply(null, 1, "value",  new IdentityParser[String])
			}
		}
	}
	describe("Builder.flatMapKey") {
		it ("The new builder's init is the same as the old builder's init") {
			val builder = new ThrowBuilder[Int, String]
			assertResult(builder.init){builder.flatMapKey[Int]{case x => Right(x)}.init}
		}
		it ("when a success, continue being a success") {
			assertResult(Right("key", "value")){
				new ReportKeyValueBuilder[String, String]
						.flatMapKey[Int]{case x => Right("key")}
						.apply(null, 1, "value",  new IdentityParser[String])
			}
		}
		it ("passes through a parser's failure") {
			assertResult(Left("FailureParser", 0)){
				new ReportKeyValueBuilder[String, String]
						.flatMapKey[Int]{case x => Right("key")}
						.apply(null, 1, "value",  new FailureParser)
			}
		}
		it ("passes through a builder's failure") {
			assertResult(Left("using ThrowBuilder::apply", 0)){
				new ThrowBuilder[String, String]
						.flatMapKey[Int]{case x => Right("key")}
						.apply(null, 1, "value",  new IdentityParser[String])
			}
		}
		it ("passes through a fun's failure") {
			assertResult(Left("???", 0)){
				new ReportKeyValueBuilder[String, String]
						.flatMapKey[Int]{case x => Left(("???", 0))}
						.apply(null, 1, "value",  new IdentityParser[String])
			}
		}
	}
	describe("Builder.mapValue") {
		it ("The new builder's init is the same as the old builder's init") {
			val builder = new ThrowBuilder[Int, String]
			assertResult(builder.init){builder.mapValue[String]{case x => x}.init}
		}
		it ("when a success, continue being a success") {
			assertResult(Right("key", "value")){
				new ReportKeyValueBuilder[String, String]
						.mapValue[Int]{case x => "value"}
						.apply(null, "key", 1,  new IdentityParser[Int])
			}
		}
		it ("passes through a parser's failure") {
			assertResult(Left("FailureParser", 0)){
				new ReportKeyValueBuilder[String, String]
						.mapValue[Int]{case x => "value"}
						.apply(null, "key", 1,  new FailureParser)
			}
		}
		it ("passes through a builder's failure") {
			assertResult(Left("using ThrowBuilder::apply", 0)){
				new ThrowBuilder[String, String]
						.mapValue[Int]{case x => "value"}
						.apply(null, "key", 1,  new IdentityParser[Int])
			}
		}
	}
	describe("Builder.flatMapValue") {
		it ("The new builder's init is the same as the old builder's init") {
			val builder = new ThrowBuilder[Int, String]
			assertResult(builder.init){builder.flatMapValue[String]{case x => Right(x)}.init}
		}
		it ("when a success, continue being a success") {
			assertResult(Right("key", "value")){
				new ReportKeyValueBuilder[String, String]
						.flatMapValue[Int]{case x => Right("value")}
						.apply(null, "key", 1,  new IdentityParser[Int])
			}
		}
		it ("passes through a parser's failure") {
			assertResult(Left("FailureParser", 0)){
				new ReportKeyValueBuilder[String, String]
						.flatMapValue[Int]{case x => Right("value")}
						.apply(null, "key", 1,  new FailureParser)
			}
		}
		it ("passes through a builder's failure") {
			assertResult(Left("using ThrowBuilder::apply", 0)){
				new ThrowBuilder[String, String]
						.flatMapValue[Int]{case x => Right("value")}
						.apply(null, "key", 1,  new IdentityParser[Int])
			}
		}
		it ("passes through a fun's failure") {
			assertResult(Left("???", 0)){
				new ReportKeyValueBuilder[String, String]
						.flatMapValue[Int]{case x => Left("???", 0)}
						.apply(null, "key", 1,  new IdentityParser[Int])
			}
		}
		
		
		
		it ("nesting") {
			import com.rayrobdod.json.union.{StringOrInt, JsonValue}
			import com.rayrobdod.json.union.JsonValue.JsonValueNumber
			
			assertResult(Right(Seq(Seq(1, 2)))){
				new SeqBuilder[StringOrInt, Int, Seq[Int]](new PrimitiveSeqBuilder[Int])
					.flatMapValue[JsonValue]{v => v match {case JsonValueNumber(x) => Right(x.intValue); case _ => Left("unexpected value", 0)}}
					.apply(Seq.empty, 0, new com.rayrobdod.json.parser.CountingReader(new java.io.StringReader("[1,2]")), new com.rayrobdod.json.parser.JsonParser)
			}
		}
	}
	
}

