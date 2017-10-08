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
import com.rayrobdod.json.parser.{Parser, FailureParser, IdentityParser, PrimitiveSeqParser}
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
		describe ("init") {
			it ("is the same as Builder.init") {
				val builder = new ThrowBuilder(EnforcedFailure)
				assertResult(builder.init){builder.mapKey[Int]{x => x}.init}
			}
		}
		describe ("apply") {
			it ("when a success, continue being a success") {
				assertResult(Complex(("key", "value"))){
					val builder = new ReportKeyValueBuilder[String, String]
							.mapKey[Int]{x => "key"}
					builder.apply(builder.init, 1, "value", new IdentityParser[String], "extra")
				}
			}
			it ("passes through a parser's failure") {
				assertResult(ParserFailure(EnforcedFailure)){
					val builder = new ReportKeyValueBuilder[String, String]
							.mapKey[Int]{x => "key"}
					builder.apply(builder.init, 1, "value", new FailureParser(EnforcedFailure), "extra")
				}
			}
			it ("passes through a builder's failure") {
				assertResult(BuilderFailure(EnforcedFailure, "extra")){
					val builder = new ThrowBuilder(EnforcedFailure)
							.mapKey[Int]{x => "key"}
					builder.apply(builder.init, 1, "value", new IdentityParser[String], "extra")
				}
			}
		}
		describe ("finish") {
			it ("is a success if Builder.finish would have been a success") {
				val builder = new ReportKeyValueBuilder[String, String]
						.mapKey[Int]{x => "key"}
				val middle = builder.apply(builder.init, 1, "value", new IdentityParser[String], "extra").fold({x => x}, {x => ???}, {x => ???}, {(x,y) => ???})
				assertResult( Complex(("key", "value")) ){ builder.finish("extra")(middle) }
			}
			it ("is a failure if Builder.finish would have been a failure") {
				val builder = new ThrowBuilder(EnforcedFailure)
						.mapKey[Int]{x => "key"}
				assertResult( BuilderFailure(EnforcedFailure, "extra") ){ builder.finish("extra")(builder.init) }
			}
		}
	}
	describe("Builder.flatMapKey") {
		describe ("init") {
			it ("is the same as Builder.init") {
				val builder = new ThrowBuilder(EnforcedFailure)
				assertResult(builder.init){builder.flatMapKey[Int, Nothing]{x => Right(x)}.init}
			}
		}
		describe ("apply") {
			it ("when a success, continue being a success") {
				assertResult(Complex("key", "value")){
					val builder = new ReportKeyValueBuilder[String, String]
							.flatMapKey[Int, Nothing]{x => Right("key")}
					builder.apply(builder.init, 1, "value", new IdentityParser[String], ())
				}
			}
			it ("passes through a parser's failure") {
				assertResult(ParserFailure(EnforcedFailure)){
					val builder = new ReportKeyValueBuilder[String, String]
							.flatMapKey[Int, Nothing]{x => Right("key")}
					builder.apply(builder.init, 1, "value", new FailureParser(EnforcedFailure), ())
				}
			}
			it ("passes through a builder's failure") {
				assertResult(BuilderFailure(Right(EnforcedFailure), ())){
					val builder = new ThrowBuilder(EnforcedFailure)
							.flatMapKey[Int, Nothing]{x => Right("key")}
					builder.apply(builder.init, 1, "value", new IdentityParser[String], ())
				}
			}
			it ("passes through a fun's failure") {
				assertResult(BuilderFailure(Left(EnforcedFailure), ())){
					val builder = new ReportKeyValueBuilder[String, String]
							.flatMapKey[Int, EnforcedFailure.type]{x => Left(EnforcedFailure)}
					builder.apply(builder.init, 1, "value", new IdentityParser[String], ())
				}
			}
		}
		describe ("finish") {
			it ("is a success if Builder.finish would have been a success") {
				val builder = new ReportKeyValueBuilder[String, String]
						.flatMapKey[Int, Nothing]{x => Right("key")}
				val middle = builder.apply(builder.init, 1, "value", new IdentityParser[String], "extra").fold({x => x}, {x => ???}, {x => ???}, {(x,y) => ???})
				assertResult( Complex(("key", "value")) ){ builder.finish("extra")(middle) }
			}
			it ("is a failure if Builder.finish would have been a failure") {
				val builder = new ThrowBuilder(EnforcedFailure)
						.flatMapKey[Int, Nothing]{x => Right("key")}
				assertResult( BuilderFailure(Right(EnforcedFailure), "extra") ){ builder.finish("extra")(builder.init) }
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
		describe ("init") {
			it ("is the same as Builder.init") {
				val builder = new ThrowBuilder(EnforcedFailure)
				assertResult(builder.init){builder.mapValue[String]{x => x}.init}
			}
		}
		describe ("apply") {
			it ("when a success, continue being a success") {
				assertResult(Complex("key", "value")){
					val builder = new ReportKeyValueBuilder[String, String]
							.mapValue[Int]{x => "value"}
					builder.apply(builder.init, "key", 1, new IdentityParser[Int], ())
				}
			}
			it ("passes through a parser's failure") {
				assertResult(ParserFailure(EnforcedFailure)){
					val builder = new ReportKeyValueBuilder[String, String]
							.mapValue[Int]{x => "value"}
					builder.apply(builder.init, "key", 1, new FailureParser(EnforcedFailure), ())
				}
			}
			it ("passes through a builder's failure") {
				assertResult(BuilderFailure(EnforcedFailure, ())){
					val builder = new ThrowBuilder(EnforcedFailure)
							.mapValue[Int]{x => "value"}
					builder.apply(builder.init, "key", 1, new IdentityParser[Int], ())
				}
			}
		}
		describe ("finish") {
			it ("is a success if Builder.finish would have been a success") {
				val builder = new ReportKeyValueBuilder[String, String]
						.mapValue[Int]{x => "value"}
				val middle = builder.apply(builder.init, "key", 1, new IdentityParser[Int], "extra").fold({x => x}, {x => ???}, {x => ???}, {(x,y) => ???})
				assertResult( Complex(("key", "value")) ){ builder.finish("extra")(middle) }
			}
			it ("is a failure if Builder.finish would have been a failure") {
				val builder = new ThrowBuilder(EnforcedFailure)
						.mapValue[Int]{x => "key"}
				assertResult( BuilderFailure(EnforcedFailure, "extra") ){ builder.finish("extra")(builder.init) }
			}
		}
	}
	describe("Builder.flatMapValue") {
		describe ("init") {
			it ("is the same as Builder.init") {
				val builder = new ThrowBuilder(EnforcedFailure)
				assertResult(builder.init){builder.flatMapValue[Int, Nothing]{x => Right(x)}.init}
			}
		}
		describe ("apply") {
			it ("when a success, continue being a success") {
				assertResult(Complex("key", "value")){
					val builder = new ReportKeyValueBuilder[String, String]
							.flatMapValue[Int, Nothing]{x => Right("value")}
					builder.apply(builder.init, "key", 1, new IdentityParser[Int], ())
				}
			}
			it ("passes through a parser's failure") {
				assertResult(ParserFailure(EnforcedFailure)){
					val builder = new ReportKeyValueBuilder[String, String]
							.flatMapValue[Int, Nothing]{x => Right("value")}
					builder.apply(builder.init, "key", 1, new FailureParser(EnforcedFailure), ())
				}
			}
			it ("passes through a builder's failure") {
				assertResult(BuilderFailure(Right(EnforcedFailure), ())){
					val builder = new ThrowBuilder(EnforcedFailure)
							.flatMapValue[Int, Nothing]{x => Right("value")}
					builder.apply(builder.init, "key", 1, new IdentityParser[Int], ())
				}
			}
			it ("passes through a fun's failure") {
				assertResult(BuilderFailure(Left(EnforcedFailure), ())){
					val builder = new ReportKeyValueBuilder[String, String]
							.flatMapValue[Int, EnforcedFailure.type]{x => Left(EnforcedFailure)}
					builder.apply(builder.init, "key", 1, new IdentityParser[Int], ())
				}
			}
		}
		describe ("finish") {
			it ("is a success if Builder.finish would have been a success") {
				val builder = new ReportKeyValueBuilder[String, String]
						.flatMapValue[Int, Nothing]{x => Right("value")}
				val middle = builder.apply(builder.init, "key", 1, new IdentityParser[Int], "extra").fold({x => x}, {x => ???}, {x => ???}, {(x,y) => ???})
				assertResult( Complex(("key", "value")) ){ builder.finish("extra")(middle) }
			}
			it ("is a failure if Builder.finish would have been a failure") {
				val builder = new ThrowBuilder(EnforcedFailure)
						.flatMapValue[Int, Nothing]{x => Right("value")}
				assertResult( BuilderFailure(Right(EnforcedFailure), "extra") ){ builder.finish("extra")(builder.init) }
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
	describe("Builder.mapFailure") {
		describe ("init") {
			it ("is the same as Builder.init") {
				val builder = new ThrowBuilder(EnforcedFailure)
				assertResult(builder.init){builder.mapFailure[String]{x => "Failure"}.init}
			}
		}
		describe ("apply") {
			it ("when a success, continue being a success") {
				assertResult(Complex("key", "value")){
					val builder = new ReportKeyValueBuilder[String, String]
							.mapFailure{x => "Failure"}
					builder.apply(builder.init, "key", "value", new IdentityParser[String], ())
				}
			}
			it ("passes through a parser's failure") {
				assertResult(ParserFailure(EnforcedFailure)){
					val builder = new ReportKeyValueBuilder[String, String]
							.mapFailure{x => "Failure"}
					builder.apply(builder.init, "key", "value", new FailureParser(EnforcedFailure), ())
				}
			}
			it ("maps a builder's failure") {
				assertResult(BuilderFailure("Failure", ())){
					val builder = new ThrowBuilder(EnforcedFailure)
							.mapFailure{x => "Failure"}
					builder.apply(builder.init, "key", "value", new IdentityParser[String], ())
				}
			}
		}
		describe ("finish") {
			it ("is a success if Builder.finish would have been a success") {
				val builder = new ReportKeyValueBuilder[String, String]
							.mapFailure{x => "Failure"}
				val middle = builder.apply(builder.init, "key", "value", new IdentityParser[String], "extra").fold({x => x}, {x => ???}, {x => ???}, {(x,y) => ???})
				assertResult( Complex(("key", "value")) ){ builder.finish("extra")(middle) }
			}
			it ("is a failure if Builder.finish would have been a failure") {
				val builder = new ThrowBuilder(EnforcedFailure)
							.mapFailure{x => "Failure"}
				assertResult( BuilderFailure("Failure", "extra") ){ builder.finish("extra")(builder.init) }
			}
		}
	}
	describe("Builder.mapResult") {
		describe ("init") {
			it ("is the same as Builder.init") {
				val builder = new ThrowBuilder(EnforcedFailure)
				assertResult(builder.init){builder.mapResult{x => (x, "postmap")}.init}
			}
		}
		describe ("apply") {
			it ("when a success, continue being a success") {
				assertResult(Complex(("key", "value"))){
					val builder = new ReportKeyValueBuilder[String, String]
							.mapResult{x => (x._1, x._2, "postmap")}
					builder.apply(builder.init, "key", "value", new IdentityParser[String], ())
				}
			}
			it ("passes through a parser's failure") {
				assertResult(ParserFailure(EnforcedFailure)){
					val builder = new ReportKeyValueBuilder[String, String]
							.mapResult{x => (x._1, x._2, "postmap")}
					builder.apply(builder.init, "key", "value", new FailureParser(EnforcedFailure), ())
				}
			}
			it ("maps a builder's failure") {
				assertResult(BuilderFailure(EnforcedFailure, ())){
					val builder = new ThrowBuilder(EnforcedFailure)
							.mapResult{x:(String, String) => (x._1, x._2, "postmap")}
					builder.apply(builder.init, "key", "value", new IdentityParser[String], ())
				}
			}
		}
		describe ("finish") {
			it ("is a success if Builder.finish would have been a success") {
				val builder = new ReportKeyValueBuilder[String, String]
							.mapResult{x => (x._1, x._2, "postmap")}
				val middle = builder.apply(builder.init, "key", "value", new IdentityParser[String], "extra").fold({x => x}, {x => ???}, {x => ???}, {(x,y) => ???})
				assertResult( Complex(("key", "value", "postmap")) ){ builder.finish("extra")(middle) }
			}
			it ("is a failure if Builder.finish would have been a failure") {
				val builder = new ThrowBuilder(EnforcedFailure)
							.mapResult{x:(String, String) => (x._1, x._2, "postmap")}
				assertResult( BuilderFailure(EnforcedFailure, "extra") ){ builder.finish("extra")(builder.init) }
			}
		}
	}
	describe("Builder.flatMapResult") {
		describe ("init") {
			it ("is the same as Builder.init") {
				val builder = new ThrowBuilder(EnforcedFailure)
				assertResult(builder.init){builder.flatMapResult{x => Right((x, "postmap"))}.init}
			}
		}
		describe ("apply") {
			it ("when a success, continue being a success") {
				assertResult(Complex("key", "value")){
					val builder = new ReportKeyValueBuilder[String, String]
							.flatMapResult{x => Right((x._1, x._2, "postmap"))}
					builder.apply(builder.init, "key", "value", new IdentityParser[String], ())
				}
			}
			it ("passes through a parser's failure") {
				assertResult(ParserFailure(EnforcedFailure)){
					val builder = new ReportKeyValueBuilder[String, String]
							.flatMapResult{x => Right((x._1, x._2, "postmap"))}
					builder.apply(builder.init, "key", "value", new FailureParser(EnforcedFailure), ())
				}
			}
			it ("passes through a builder's failure") {
				assertResult(BuilderFailure(Right(EnforcedFailure), ())){
					val builder = new ThrowBuilder(EnforcedFailure)
							.flatMapResult{x:(String, String) => Right((x._1, x._2, "postmap"))}
					builder.apply(builder.init, "key", "value", new IdentityParser[String], ())
				}
			}
			it ("a fun's failure is irrelevant") {
				assertResult(Complex("key", "value")){
					val builder = new ReportKeyValueBuilder[String, String]
							.flatMapResult{x => Left(EnforcedFailure)}
					builder.apply(builder.init, "key", "value", new IdentityParser[String], ())
				}
			}
		}
		describe ("finish") {
			it ("is a success if Builder.finish would have been a success") {
				val builder = new ReportKeyValueBuilder[String, String]
						.flatMapResult{x => Right((x._1, x._2, "postmap"))}
				val middle = builder.apply(builder.init, "key", "value", new IdentityParser[String], "extra").fold({x => x}, {x => ???}, {x => ???}, {(x,y) => ???})
				assertResult( Complex(("key", "value", "postmap")) ){ builder.finish("extra")(middle) }
			}
			it ("is a failure if Builder.finish would have been a failure") {
				val builder = new ThrowBuilder(EnforcedFailure)
						.flatMapResult{x:(String, String) => Right((x._1, x._2, "postmap"))}
				assertResult( BuilderFailure(Right(EnforcedFailure), "extra") ){ builder.finish("extra")(builder.init) }
			}
			it ("passes through a fun's failure") {
				val builder = new ReportKeyValueBuilder[String, String]
						.flatMapResult{x => Left(EnforcedFailure)}
				val middle = builder.apply(builder.init, "key", "value", new IdentityParser[String], ()).fold({x => x}, {x => ???}, {x => ???}, {(x,y) => ???})
				assertResult(BuilderFailure(Left(EnforcedFailure), "extra")){ builder.finish("extra")(middle) }
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
		import com.rayrobdod.json.parser._
		import com.rayrobdod.json.union._
		val cborBuilder = new CborBuilder()
				.mapKey[StringOrInt].mapValue[JsonValue]
		val jsonBuilder = new PrettyJsonBuilder(PrettyJsonBuilder.MinifiedPrettyParams)
		val failureBuilder = new ThrowBuilder(UnsuccessfulTypeCoercion)
				.mapKey[StringOrInt].mapValue[JsonValue]
				.mapFailure[PiecewiseBuilder.Failures]
		val primitiveBuilder = new ReportKeyValueBuilder[StringOrInt, JsonValue]
				.mapFailure[PiecewiseBuilder.Failures]
		
		describe ("init") {
			it ("asdf") {
				val exp = cborBuilder.init -> jsonBuilder.init
				val res = (cborBuilder zip jsonBuilder).init
				assertResult(exp){res}
			}
		}
		describe ("apply") {
			it ("success zip success -> success (primitive)") {
				val exp = Complex(((StringOrInt("key"), JsonValue("value")), (StringOrInt("key"), JsonValue("value"))))
				val zipBuilder = primitiveBuilder zip primitiveBuilder
				val res = zipBuilder.apply(zipBuilder.init, "key", JsonValue("value"), new IdentityParser[JsonValue], ())
				assertResult(exp){res}
			}
			it ("failure zip success -> failure (primitive)") {
				val exp = BuilderFailure(UnsuccessfulTypeCoercion, "extra")
				val zipBuilder = failureBuilder zip primitiveBuilder
				val res = zipBuilder.apply(zipBuilder.init, "key", JsonValue("value"), new IdentityParser[JsonValue], "extra")
				assertResult(exp){res}
			}
			it ("success zip failure -> failure (primitive)") {
				val exp = BuilderFailure(UnsuccessfulTypeCoercion, "extra")
				val zipBuilder = primitiveBuilder zip failureBuilder
				val res = zipBuilder.apply(zipBuilder.init, "key", JsonValue("value"), new IdentityParser[JsonValue], "extra")
				assertResult(exp){res}
			}
			
			it ("success zip success -> success (complex)") {
				val exp = {
					val thing = CborBuilder.Middle(true,1,List(hexSeq"636b6579826476616c316476616c32"))
					Complex((thing, thing))
				}
				val zipBuilder = cborBuilder zip cborBuilder
				val res = zipBuilder.apply(zipBuilder.init, "key", Seq("val1", "val2"), PrimitiveSeqParser[StringOrInt, JsonValue, String], ())
				assertResult(exp){res}
			}
			it ("failure zip success -> failure (complex)") {
				val exp = BuilderFailure(UnsuccessfulTypeCoercion, "extra")
				val zipBuilder = failureBuilder zip cborBuilder
				val res = zipBuilder.apply(zipBuilder.init, "key", Seq("val1", "val2"), PrimitiveSeqParser[StringOrInt, JsonValue, String], "extra")
				assertResult(exp){res}
			}
			it ("success zip failure -> failure (complex)") {
				val exp = BuilderFailure(UnsuccessfulTypeCoercion, "extra")
				val zipBuilder = cborBuilder zip failureBuilder
				val res = zipBuilder.apply(zipBuilder.init, "key", Seq("val1", "val2"), PrimitiveSeqParser[StringOrInt, JsonValue, String], "extra")
				assertResult(exp){res}
			}
			
			it ("passes through a parser's failure") {
				val exp = ParserFailure(EnforcedFailure)
				val zipBuilder = primitiveBuilder zip primitiveBuilder
				val res = zipBuilder.apply(zipBuilder.init, "key", JsonValue("value"), new FailureParser(EnforcedFailure), "extra")
				assertResult(exp){res}
			}
		}
		describe ("finish") {
			it ("success zip success -> success") {
				val exp = Complex((hexSeq"80", "[]"))
				val zipBuilder = cborBuilder zip jsonBuilder
				val res = zipBuilder.finish("extra")(zipBuilder.init)
				assertResult(exp){res}
			}
			it ("failure zip success -> failure") {
				val exp = BuilderFailure(UnsuccessfulTypeCoercion, "extra")
				val zipBuilder = failureBuilder zip cborBuilder
				val res = zipBuilder.finish("extra")(zipBuilder.init)
				assertResult(exp){res}
			}
			it ("success zip failure -> failure") {
				val exp = BuilderFailure(UnsuccessfulTypeCoercion, "extra")
				val zipBuilder = cborBuilder zip failureBuilder
				val res = zipBuilder.finish("extra")(zipBuilder.init)
				assertResult(exp){res}
			}
		}
	}
}
