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
import com.rayrobdod.json.builder.PiecewiseBuilder.Failures.{ExpectedPrimitive, UnsuccessfulTypeCoercion}
import com.rayrobdod.json.testing.EnforcedFailure
import com.rayrobdod.json.parser.Parser
import com.rayrobdod.json.parser.FailureParser
import com.rayrobdod.json.parser.IdentityParser
import com.rayrobdod.json.union.ParserRetVal
import com.rayrobdod.json.union.ParserRetVal.{BuilderFailure, Complex, ParserFailure}
import com.rayrobdod.json.testing.HexArrayStringConverter

class BuilderTest extends FunSpec {
	
	private[this] final class ReportKeyValueBuilder[A,B] extends Builder[A,B,ExpectedPrimitive.type,(A,B)] {
		override type Middle = (A,B)
		def init = null
		def apply[Input,BF,EX](folding:(A,B), key:A, input:Input, parser:Parser[A, B, BF, EX, Input], ex:EX):ParserRetVal[(A,B), Nothing, BF, ExpectedPrimitive.type, EX] = {
			parser.parsePrimitive(input, ExpectedPrimitive).primitive.flatMap{p => Complex( (key, p) )}
		}
		def finish[EX](extra:EX)(x:Middle) = ParserRetVal.Complex(x)
	}
	
	
	describe("Builder.mapKey") {
		it ("The new builder's init is the same as the old builder's init") {
			val builder = new ThrowBuilder(EnforcedFailure)
			assertResult(builder.init){builder.mapKey[Int]{x => x}.init}
		}
		it ("when a success, continue being a success") {
			assertResult(Complex("key", "value")){
				val builder = new ReportKeyValueBuilder[String, String]
						.mapKey[Int]{case x => "key"}
				builder.apply(builder.init, 1, "value",  new IdentityParser[String], ())
			}
		}
		it ("passes through a parser's failure") {
			assertResult(ParserFailure(EnforcedFailure)){
				val builder = new ReportKeyValueBuilder[String, String]
						.mapKey[Int]{case x => "key"}
				builder.apply(builder.init, 1, "value",  new FailureParser(EnforcedFailure), ())
			}
		}
		it ("passes through a builder's failure") {
			assertResult(BuilderFailure(EnforcedFailure, ())){
				val builder = new ThrowBuilder(EnforcedFailure)
						.mapKey[Int]{case x => "key"}
				builder.apply(builder.init, 1, "value",  new IdentityParser[String], ())
			}
		}
	}
	describe("Builder.flatMapKey") {
		it ("The new builder's init is the same as the old builder's init") {
			val builder = new ThrowBuilder(EnforcedFailure)
			assertResult(builder.init){builder.flatMapKey[Int, Nothing]{case x => Right(x)}.init}
		}
		it ("when a success, continue being a success") {
			assertResult(Complex("key", "value")){
				val builder = new ReportKeyValueBuilder[String, String]
						.flatMapKey[Int, Nothing]{case x => Right("key")}
				builder.apply(builder.init, 1, "value",  new IdentityParser[String], ())
			}
		}
		it ("passes through a parser's failure") {
			assertResult(ParserFailure(EnforcedFailure)){
				val builder = new ReportKeyValueBuilder[String, String]
						.flatMapKey[Int, Nothing]{case x => Right("key")}
				builder.apply(builder.init, 1, "value",  new FailureParser(EnforcedFailure), ())
			}
		}
		it ("passes through a builder's failure") {
			assertResult(BuilderFailure(Right(EnforcedFailure), ())){
				val builder = new ThrowBuilder(EnforcedFailure)
						.flatMapKey[Int, Nothing]{case x => Right("key")}
				builder.apply(builder.init, 1, "value",  new IdentityParser[String], ())
			}
		}
		it ("passes through a fun's failure") {
			assertResult(BuilderFailure(Left(EnforcedFailure), ())){
				val builder = new ReportKeyValueBuilder[String, String]
						.flatMapKey[Int, EnforcedFailure.type]{case x => Left(EnforcedFailure)}
				builder.apply(builder.init, 1, "value",  new IdentityParser[String], ())
			}
		}
		
		it ("when an inner builder fails, get the inner parser's extra") {
			import com.rayrobdod.json.parser._
			import com.rayrobdod.json.union._
			
			val parser = new SeqParser(new PrimitiveSeqParser[Int].mapExtra{x:Unit => "innerExtra"})(implicitly, {x:Unit => "outerExtra"})
			val throwIf23Builder = MapBuilder[Int, Int]
					.flatMapKey{x:Int => if (x == 1) {Left(EnforcedFailure)} else {Right(x)}}
			
			assertResult(ParserRetVal.BuilderFailure(Left(EnforcedFailure), "innerExtra")){
				parser.parse(throwIf23Builder, Seq(Seq(1,23,3), Seq(4,5,6)))
			}
		}
	}
	describe("Builder.mapValue") {
		it ("The new builder's init is the same as the old builder's init") {
			val builder = new ThrowBuilder(EnforcedFailure)
			assertResult(builder.init){builder.mapValue[String]{case x => x}.init}
		}
		it ("when a success, continue being a success") {
			assertResult(Complex("key", "value")){
				val builder = new ReportKeyValueBuilder[String, String]
						.mapValue[Int]{case x => "value"}
				builder.apply(builder.init, "key", 1,  new IdentityParser[Int], ())
			}
		}
		it ("passes through a parser's failure") {
			assertResult(ParserFailure(EnforcedFailure)){
				val builder = new ReportKeyValueBuilder[String, String]
						.mapValue[Int]{case x => "value"}
				builder.apply(builder.init, "key", 1,  new FailureParser(EnforcedFailure), ())
			}
		}
		it ("passes through a builder's failure") {
			assertResult(BuilderFailure(EnforcedFailure, ())){
				val builder = new ThrowBuilder(EnforcedFailure)
						.mapValue[Int]{case x => "value"}
				builder.apply(builder.init, "key", 1,  new IdentityParser[Int], ())
			}
		}
	}
	describe("Builder.flatMapValue") {
		it ("The new builder's init is the same as the old builder's init") {
			val builder = new ThrowBuilder(EnforcedFailure)
			assertResult(builder.init){builder.flatMapValue[String, EnforcedFailure.type]{case x => Right(x)}.init}
		}
		it ("when a success, continue being a success") {
			assertResult(Complex("key", "value")){
				val builder = new ReportKeyValueBuilder[String, String]
						.flatMapValue[Int, EnforcedFailure.type]{case x => Right("value")}
				builder.apply(builder.init, "key", 1,  new IdentityParser[Int], ())
			}
		}
		it ("passes through a parser's failure") {
			assertResult(ParserFailure(EnforcedFailure)){
				val builder = new ReportKeyValueBuilder[String, String]
						.flatMapValue[Int, EnforcedFailure.type]{case x => Right("value")}
				builder.apply(builder.init, "key", 1,  new FailureParser(EnforcedFailure), ())
			}
		}
		it ("passes through a builder's failure") {
			assertResult(BuilderFailure(Right(EnforcedFailure), ())){
				val builder = new ThrowBuilder(EnforcedFailure)
						.flatMapValue[Int, EnforcedFailure.type]{case x => Right("value")}
				builder.apply(builder.init, "key", 1,  new IdentityParser[Int], ())
			}
		}
		it ("passes through a fun's failure") {
			assertResult(BuilderFailure(Left(EnforcedFailure), ())){
				val builder = new ReportKeyValueBuilder[String, String]
						.flatMapValue[Int, EnforcedFailure.type]{case x => Left(EnforcedFailure)}
				builder.apply(builder.init, "key", 1,  new IdentityParser[Int], ())
			}
		}
		
		
		
		it ("nesting (success)") {
			import com.rayrobdod.json.union.{StringOrInt, JsonValue}
			
			assertResult(Complex(Seq(Seq(1, 2)))){
				val builder = new SeqBuilder[StringOrInt, Int, ExpectedPrimitive.type, Seq[Int]](new PrimitiveSeqBuilder(ExpectedPrimitive), ExpectedPrimitive)
					.flatMapValue[JsonValue, UnsuccessfulTypeCoercion.type]{_.integerToEither{x => Right(x)}.left.map{x => UnsuccessfulTypeCoercion}}
				builder.apply(builder.init, 0, new com.rayrobdod.json.parser.CountingReader(new java.io.StringReader("[1,2]")), new com.rayrobdod.json.parser.JsonParser, com.rayrobdod.json.parser.CharacterIndex(0))
			}
		}
		it ("when an inner builder fails, get the inner parser's extra") {
			import com.rayrobdod.json.parser._
			import com.rayrobdod.json.union._
			
			val parser = new SeqParser(new PrimitiveSeqParser[Int].mapExtra{x:Unit => "innerExtra"})(implicitly, {x:Unit => "outerExtra"})
			val builder = MapBuilder[Int, Int]
					.flatMapValue{x:Int => if (x == 23) {Left(EnforcedFailure)} else {Right(x)}}
			
			assertResult(ParserRetVal.BuilderFailure(Left(EnforcedFailure), "innerExtra")){
				parser.parse(builder, Seq(Seq(1,23,3), Seq(4,5,6)))
			}
		}
	}
	describe("Builder.mapResult") {
		it ("The new builder's init is the same as the old builder's init") {
			val builder = new ThrowBuilder(EnforcedFailure)
			assertResult(builder.init){builder.flatMapResult[String, EnforcedFailure.type]{case x => Right(x)}.init}
		}
		it ("when a success, continue being a success") {
			assertResult(Complex("result")){
				val builder = new ReportKeyValueBuilder[String, Int]
						.mapResult[String]{x => "result"}
				builder.finish(())(builder.init)
			}
		}
		it ("passes through a builder's failure") {
			assertResult(BuilderFailure(EnforcedFailure, ())){
				val builder = new ThrowBuilder(EnforcedFailure)
						.mapResult[String]{x => "result"}
				builder.finish(())(builder.init)
			}
		}
	}
	describe("Builder.flatMapResult") {
		it ("The new builder's init is the same as the old builder's init") {
			val builder = new ThrowBuilder(EnforcedFailure)
			assertResult(builder.init){builder.flatMapResult[String, EnforcedFailure.type]{case x => Right(x)}.init}
		}
		it ("when a success, continue being a success") {
			assertResult(Complex("result")){
				val builder = new ReportKeyValueBuilder[String, Int]
						.flatMapResult[String, EnforcedFailure.type]{case x => Right("result")}
				builder.finish(())(builder.init)
			}
		}
		it ("passes through a builder's failure") {
			assertResult(BuilderFailure(Right(EnforcedFailure), ())){
				val builder = new ThrowBuilder(EnforcedFailure)
						.flatMapResult[String, EnforcedFailure.type]{case x => Right("result")}
				builder.finish(())(builder.init)
			}
		}
		it ("passes through a fun's failure") {
			assertResult(BuilderFailure(Left(EnforcedFailure), ())){
				val builder = new ReportKeyValueBuilder[String, Int]
						.flatMapResult[String, EnforcedFailure.type]{case x => Left(EnforcedFailure)}
				builder.finish(())(builder.init)
			}
		}
		
		it ("when an inner builder fails, get the inner parser's extra") {
			import com.rayrobdod.json.parser._
			import com.rayrobdod.json.union._
			
			val parser = new SeqParser(new PrimitiveSeqParser[Int].mapExtra{x:Unit => "innerExtra"})(implicitly, {x:Unit => "outerExtra"})
			val builder = new SeqBuilder(new PrimitiveSeqBuilder[Int, String]("innerError").flatMapResult{x => Left("innerError2")}, "outerrError")
					.flatMapResult{x:Seq[Seq[Int]] => Right(x)}
			
			assertResult(ParserRetVal.BuilderFailure(Right(Left("innerError2")), "innerExtra")){
				parser.parse(builder, Seq(Seq(1,2,3), Seq(4,5,6)))
			}
		}
	}
	describe("Builder.zip") {
		it ("when a success, continue being a success (primitive)") {
			val exp = Complex((("key", "value"), ("key", "value")))
			val builder = new ReportKeyValueBuilder[String, String]
					.zip(new ReportKeyValueBuilder[String, String])
			
			assertResult(exp){
				builder.apply(builder.init, "key", "value", new IdentityParser[String], ())
			}
		}
		it ("when a success, continue being a success (complex)") {
			import com.rayrobdod.json.parser._
			import com.rayrobdod.json.union._
			val failureMapping = {x:IllegalFoldingFailure.type => com.rayrobdod.json.builder.PrettyJsonBuilder.Failures.ArrayKeyNotIncrementing(0, 0)}
			val exp = ParserRetVal.Complex( ((hexSeq"818163616263" ,"""[["abc"]]""")) )
			val builder = new CborBuilder().mapKey[StringOrInt].mapValue[JsonValue].mapFailure(failureMapping)
				.zip(new PrettyJsonBuilder(PrettyJsonBuilder.MinifiedPrettyParams))
			
			val res = new JsonParser().parse(builder, """[ [ "abc" ] ]""")
			assertResult(exp){res}
		}
	}
	
}
