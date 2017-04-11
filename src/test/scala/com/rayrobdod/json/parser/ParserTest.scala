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
package com.rayrobdod.json.parser

import org.scalatest.FunSpec
import com.rayrobdod.json.builder.{Builder, ThrowBuilder, MapBuilder}
import com.rayrobdod.json.builder.PiecewiseBuilder.Failures.UnsuccessfulTypeCoercion
import com.rayrobdod.json.testing.EnforcedFailure
import com.rayrobdod.json.union.ParserRetVal
import com.rayrobdod.json.union.ParserRetVal.{BuilderFailure, Complex, ParserFailure, Primitive}

final class ParserTest extends FunSpec {
	
	object MyParserExtra
	
	final class SingleKeyValueParser[K,V] extends Parser[K,V,Nothing,MyParserExtra.type,(K,V)] {
		def parse[A,BF](b:Builder[K,V,BF,A], kv:(K,V)):ParserRetVal[A,Nothing,Nothing,BF,MyParserExtra.type] = {
			b.apply(b.init, kv._1, kv._2, new IdentityParser[V], ())
					.builderFailure.attachExtra(MyParserExtra)
					.complex.flatMap{b.finish(MyParserExtra)}
		}
	}
	
	def doThing[K, V, PF, BF, BFE, Input, Result](
			  expected:ParserRetVal[Result, V, PF, BF, BFE]
			, parser:Parser[K,V,PF,BFE,Input]
			, builder:Builder[K,V,BF,Result]
			, input:Input
	) {
		val result = parser.parse(builder, input)
		assertResult(expected){result}
	}
	
	describe("Parser.mapKey") {
		it ("when a success, continue being a success") {
			doThing(
				  Complex(Map("key" -> Right("value")))
				, new SingleKeyValueParser[Int, String]
						.mapKey{x:Int => "key"}
				, MapBuilder.apply[String, String]
				, ((23 -> "value"))
			)
		}
		it ("will pass through a parser's failure") {
			doThing(
				  ParserFailure(EnforcedFailure)
				, new FailureParser(EnforcedFailure)
						.mapKey{x:Int => "key"}
				, MapBuilder.apply[String, String]
				, ((23 -> "value"))
			)
		}
		it ("will pass through a builder's failure") {
			doThing(
				  BuilderFailure(EnforcedFailure, MyParserExtra)
				, new SingleKeyValueParser[Int, String]
						.mapKey{x:Int => "key"}
				, new ThrowBuilder(EnforcedFailure)
				, ((23 -> "value"))
			)
		}
	}
	describe("Parser.flatMapKey") {
		it ("when a success, continue being a success") {
			doThing(
				  Complex(Map("key" -> Right("value")))
				, new SingleKeyValueParser[Int, String]
						.flatMapKey{x:Int => Right("key")}
				, MapBuilder.apply[String, String]
				, ((23 -> "value"))
			)
		}
		it ("will pass through a parser's failure") {
			doThing(
				  ParserFailure(Right(EnforcedFailure))
				, new FailureParser(EnforcedFailure)
						.flatMapKey{x:Int => Right("key")}
				, MapBuilder.apply[String, String]
				, ((23 -> "value"))
			)
		}
		it ("will pass through a builder's failure") {
			doThing(
				  BuilderFailure(EnforcedFailure, MyParserExtra)
				, new SingleKeyValueParser[Int, String]
						.flatMapKey{x:Int => Right("key")}
				, new ThrowBuilder(EnforcedFailure)
				, ((23 -> "value"))
			)
		}
		it ("will pass through a mapping function failure") {
			doThing(
				  ParserFailure(Left(EnforcedFailure))
				, new SingleKeyValueParser[Int, String]
						.flatMapKey{x:Int => Left(EnforcedFailure)}
				, MapBuilder.apply[String, String]
				, ((23 -> "value"))
			)
		}
	}
	describe("Parser.mapValue") {
		it ("when a success, continue being a success") {
			doThing(
				  Complex(Map("key" -> Right("value")))
				, new SingleKeyValueParser[String, Int]
						.mapValue{x:Int => "value"}
				, MapBuilder.apply[String, String]
				, (("key" -> 59))
			)
		}
		it ("will pass through a parser's failure") {
			doThing(
				  ParserFailure(EnforcedFailure)
				, new FailureParser(EnforcedFailure)
						.mapValue{x:Int => "value"}
				, MapBuilder.apply[String, String]
				, (("key" -> 59))
			)
		}
		it ("will pass through a builder's failure") {
			doThing(
				  BuilderFailure(EnforcedFailure, MyParserExtra)
				, new SingleKeyValueParser[String, Int]
						.mapValue{x:Int => "value"}
				, new ThrowBuilder(EnforcedFailure)
				, (("key" -> 59))
			)
		}
	}
	describe("Parser.flatMapValue") {
		it ("when a success, continue being a success (complex)") {
			doThing(
				  Complex(Map("key" -> Right("value")))
				, new SingleKeyValueParser[String, Int]
						.flatMapValue{x:Int => Right("value")}
				, MapBuilder.apply[String, String]
				, (("key" -> 59))
			)
		}
		it ("will pass through a parser's failure (complex)") {
			doThing(
				  ParserFailure(Right(EnforcedFailure))
				, new FailureParser(EnforcedFailure)
						.flatMapValue{x:Int => Right("value")}
				, MapBuilder.apply[String, String]
				, (("key" -> 59))
			)
		}
		it ("will pass through a builder's failure (complex)") {
			doThing(
				  BuilderFailure(EnforcedFailure, MyParserExtra)
				, new SingleKeyValueParser[String, Int]
						.flatMapValue{x:Int => Right("value")}
				, new ThrowBuilder(EnforcedFailure)
				, (("key" -> 59))
			)
		}
		it ("will pass through a mapping function failure (complex)") {
			doThing(
				  ParserFailure(Left(EnforcedFailure))
				, new SingleKeyValueParser[String, Int]
						.flatMapValue{x:Int => Left(EnforcedFailure)}
				, MapBuilder.apply[String, String]
				, (("key" -> 59))
			)
		}
		
		it ("when a success, continue being a success (primitive)") {
			doThing(
				  Primitive("value")
				, IdentityParser[Int]
						.flatMapValue{x:Int => Right("value")}
				, MapBuilder.apply[String, String]
				, 101
			)
		}
		it ("will pass through a parser's failure (primitive)") {
			doThing(
				  ParserFailure(Right(EnforcedFailure))
				, new FailureParser(EnforcedFailure)
						.flatMapValue{x:Int => Right("value")}
				, MapBuilder.apply[String, String]
				, 101
			)
		}
		it ("will pass through a mapping function failure (primitive)") {
			doThing(
				  ParserFailure(Left(EnforcedFailure))
				, IdentityParser[Int]
						.flatMapValue{x:Int => Left(EnforcedFailure)}
				, MapBuilder.apply[String, String]
				, 101
			)
		}
	}
	describe("Parser.mapFailure") {
		it ("when a success, continue being a success") {
			doThing(
				  Complex(Map("key" -> Right("value")))
				, new SingleKeyValueParser[String, String]
						.mapFailure{x:EnforcedFailure.type => UnsuccessfulTypeCoercion}
				, MapBuilder.apply[String, String]
				, (("key" -> "value"))
			)
		}
		it ("will change a parser's failure") {
			doThing(
				  ParserFailure(UnsuccessfulTypeCoercion)
				, new FailureParser(EnforcedFailure)
						.mapFailure{x:EnforcedFailure.type => UnsuccessfulTypeCoercion}
				, MapBuilder.apply[String, String]
				, (("key" -> "value"))
			)
		}
		it ("will pass through a builder's failure") {
			doThing(
				  BuilderFailure(EnforcedFailure, MyParserExtra)
				, new SingleKeyValueParser[String, String]
						.mapFailure{x:EnforcedFailure.type => UnsuccessfulTypeCoercion}
				, new ThrowBuilder(EnforcedFailure)
				, (("key" -> "value"))
			)
		}
	}
	describe("Parser.mapExtra") {
		it ("when a success, continue being a success") {
			doThing(
				  Complex(Map("key" -> Right("value")))
				, new SingleKeyValueParser[String, String]
						.mapExtra{x:MyParserExtra.type => "EXTRA"}
				, MapBuilder.apply[String, String]
				, (("key" -> "value"))
			)
		}
		it ("will pass through a parser's failure") {
			doThing(
				  ParserFailure(EnforcedFailure)
				, new FailureParser(EnforcedFailure)
						.mapExtra{x:Unit => "EXTRA"}
				, MapBuilder.apply[String, String]
				, (("key" -> "value"))
			)
		}
		it ("will change a builder's failure") {
			doThing(
				  BuilderFailure(EnforcedFailure, "EXTRA")
				, new SingleKeyValueParser[String, String]
						.mapExtra{x:MyParserExtra.type => "EXTRA"}
				, new ThrowBuilder(EnforcedFailure)
				, (("key" -> "value"))
			)
		}
	}
}
