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
import com.rayrobdod.json.union.ParserRetVal
import com.rayrobdod.json.union.ParserRetVal.{BuilderFailure, Complex, ParserFailure}
import com.rayrobdod.json.union.Failures.{ExpectedPrimitive, EnforcedFailure, UnsuccessfulTypeCoersion}

class BuilderTest extends FunSpec {
	
	private[this] final class ReportKeyValueBuilder[A,B] extends Builder[A,B,ExpectedPrimitive.type,(A,B)] {
		def init = null
		def apply[Input,BF](folding:(A,B), key:A, input:Input, parser:Parser[A, B, BF, Input]):ParserRetVal[(A,B), Nothing, BF, ExpectedPrimitive.type] = {
			parser.parsePrimitive(input).primitive.flatMap{p => Complex( (key, p) )}
		}
	}
	
	
	describe("Builder.mapKey") {
		it ("The new builder's init is the same as the old builder's init") {
			val builder = new ThrowBuilder[Int, String]
			assertResult(builder.init){builder.mapKey[Int]{x => x}.init}
		}
		it ("when a success, continue being a success") {
			assertResult(Complex("key", "value")){
				new ReportKeyValueBuilder[String, String]
						.mapKey[Int]{case x => "key"}
						.apply(null, 1, "value",  new IdentityParser[String])
			}
		}
		it ("passes through a parser's failure") {
			assertResult(ParserFailure(EnforcedFailure)){
				new ReportKeyValueBuilder[String, String]
						.mapKey[Int]{case x => "key"}
						.apply(null, 1, "value",  new FailureParser)
			}
		}
		it ("passes through a builder's failure") {
			assertResult(BuilderFailure(EnforcedFailure)){
				new ThrowBuilder[String, String]
						.mapKey[Int]{case x => "key"}
						.apply(null, 1, "value",  new IdentityParser[String])
			}
		}
	}
	describe("Builder.flatMapKey") {
		it ("The new builder's init is the same as the old builder's init") {
			val builder = new ThrowBuilder[Int, String]
			assertResult(builder.init){builder.flatMapKey[Int, Nothing]{case x => Right(x)}.init}
		}
		it ("when a success, continue being a success") {
			assertResult(Complex("key", "value")){
				new ReportKeyValueBuilder[String, String]
						.flatMapKey[Int, Nothing]{case x => Right("key")}
						.apply(null, 1, "value",  new IdentityParser[String])
			}
		}
		it ("passes through a parser's failure") {
			assertResult(ParserFailure(EnforcedFailure)){
				new ReportKeyValueBuilder[String, String]
						.flatMapKey[Int, Nothing]{case x => Right("key")}
						.apply(null, 1, "value",  new FailureParser)
			}
		}
		it ("passes through a builder's failure") {
			assertResult(BuilderFailure(Right(EnforcedFailure))){
				new ThrowBuilder[String, String]
						.flatMapKey[Int, Nothing]{case x => Right("key")}
						.apply(null, 1, "value",  new IdentityParser[String])
			}
		}
		it ("passes through a fun's failure") {
			assertResult(BuilderFailure(Left(EnforcedFailure))){
				new ReportKeyValueBuilder[String, String]
						.flatMapKey[Int, EnforcedFailure.type]{case x => Left(EnforcedFailure)}
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
			assertResult(Complex("key", "value")){
				new ReportKeyValueBuilder[String, String]
						.mapValue[Int]{case x => "value"}
						.apply(null, "key", 1,  new IdentityParser[Int])
			}
		}
		it ("passes through a parser's failure") {
			assertResult(ParserFailure(EnforcedFailure)){
				new ReportKeyValueBuilder[String, String]
						.mapValue[Int]{case x => "value"}
						.apply(null, "key", 1,  new FailureParser)
			}
		}
		it ("passes through a builder's failure") {
			assertResult(BuilderFailure(EnforcedFailure)){
				new ThrowBuilder[String, String]
						.mapValue[Int]{case x => "value"}
						.apply(null, "key", 1,  new IdentityParser[Int])
			}
		}
	}
	describe("Builder.flatMapValue") {
		it ("The new builder's init is the same as the old builder's init") {
			val builder = new ThrowBuilder[Int, String]
			assertResult(builder.init){builder.flatMapValue[String, EnforcedFailure.type]{case x => Right(x)}.init}
		}
		it ("when a success, continue being a success") {
			assertResult(Complex("key", "value")){
				new ReportKeyValueBuilder[String, String]
						.flatMapValue[Int, EnforcedFailure.type]{case x => Right("value")}
						.apply(null, "key", 1,  new IdentityParser[Int])
			}
		}
		it ("passes through a parser's failure") {
			assertResult(ParserFailure(EnforcedFailure)){
				new ReportKeyValueBuilder[String, String]
						.flatMapValue[Int, EnforcedFailure.type]{case x => Right("value")}
						.apply(null, "key", 1,  new FailureParser)
			}
		}
		it ("passes through a builder's failure") {
			assertResult(BuilderFailure(Right(EnforcedFailure))){
				new ThrowBuilder[String, String]
						.flatMapValue[Int, EnforcedFailure.type]{case x => Right("value")}
						.apply(null, "key", 1,  new IdentityParser[Int])
			}
		}
		it ("passes through a fun's failure") {
			assertResult(BuilderFailure(Left(EnforcedFailure))){
				new ReportKeyValueBuilder[String, String]
						.flatMapValue[Int, EnforcedFailure.type]{case x => Left(EnforcedFailure)}
						.apply(null, "key", 1,  new IdentityParser[Int])
			}
		}
		
		
		
		it ("nesting") {
			import com.rayrobdod.json.union.{StringOrInt, JsonValue}
			
			assertResult(Complex(Seq(Seq(1, 2)))){
				new SeqBuilder[StringOrInt, Int, ExpectedPrimitive.type, Seq[Int]](new PrimitiveSeqBuilder[Int])
					.flatMapValue[JsonValue, UnsuccessfulTypeCoersion]{_.integerToEither{x => Right(x)}.left.map{x => UnsuccessfulTypeCoersion(0,"","")}}
					.apply(Seq.empty, 0, new com.rayrobdod.json.parser.CountingReader(new java.io.StringReader("[1,2]")), new com.rayrobdod.json.parser.JsonParser)
			}
		}
	}
	
}
