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
import com.rayrobdod.json.union.ParserRetVal
import com.rayrobdod.json.union.ParserRetVal.Complex
import com.rayrobdod.json.union.Failures.{ExpectedPrimitive, ExpectedComplex, EnforcedFailure}

class SeqBuilderTest extends FunSpec {
	
	describe("PrimitiveSeqBuilder") {
		it ("inits correctly") {
			assertResult(Nil){new PrimitiveSeqBuilder().init}
		}
		it ("Appends value") {
			val myValue = new Object
			
			assertResult(ParserRetVal.Complex(Seq(myValue))){
				new PrimitiveSeqBuilder().apply(Nil, "sdfa", myValue, new IdentityParser[Object])
			}
		}
		it ("Appends value 2") {
			val myValue1 = new Object
			val myValue2 = new Object
			
			assertResult(ParserRetVal.Complex(Seq(myValue1, myValue2))){
				new PrimitiveSeqBuilder().apply(Seq(myValue1), "sdfa", myValue2, new IdentityParser[Object])
			}
		}
		it ("throws when builder gives it a complex value") {
			assertResult(ParserRetVal.BuilderFailure(ExpectedPrimitive)){
				new PrimitiveSeqBuilder[String].apply(Nil, 5, Seq("a","b","c"), new PrimitiveSeqParser[String])
			}
		}
	}
	describe("SeqBuilder") {
		it ("fails when builder gives it a primitive value") {
			val myValue2 = new Object
			
			assertResult(ParserRetVal.BuilderFailure(util.Left(ExpectedComplex))){
				new SeqBuilder(new PrimitiveSeqBuilder[Object]).apply(Nil, "sdfa", myValue2, new IdentityParser[Object])
			}
		}
		it ("fails when builder gives it a failure") {
			val myValue2 = new Object
			
			assertResult(ParserRetVal.BuilderFailure(util.Right(EnforcedFailure))){
				new SeqBuilder(new ThrowBuilder[Int, Object]).apply(Nil, 12, Seq(myValue2), new SeqParser(new IdentityParser[Object]))
			}
		}
		it ("fails when parser gives it a failure") {
			val myValue2 = new Object
			
			assertResult(ParserRetVal.ParserFailure(EnforcedFailure)){
				new SeqBuilder(new PrimitiveSeqBuilder[Object]).apply(Nil, "sdfa", myValue2, new FailureParser)
			}
		}
	}
	
	describe("SeqBuilder integration") {
		import com.rayrobdod.json.union.{JsonValue, StringOrInt}
		import com.rayrobdod.json.parser.JsonParser
		
		it ("PrimitiveSeqBuilder + JsonParser + primitive") {
			assertResult(Complex(Seq("a", "b", "c").map{JsonValue(_)})){
				new JsonParser().parse(
					new PrimitiveSeqBuilder[JsonValue].mapKey[StringOrInt]{StringOrInt.unwrapToString},
					"""["a", "b", "c"]"""
				)
			}
		}
		it ("PrimitiveSeqBuilder + PrimitiveSeqParser") {
			val exp = Seq(15, -4, 2)
			val res = new PrimitiveSeqParser[Int]().parse(new PrimitiveSeqBuilder[Int], exp) 
			assertResult(Complex(exp)){res}
		}
		it ("SeqBuilder + SeqParser") {
			val exp = Seq(Seq("a", "b", "c"), Seq("d", "e", "f"), Seq("g", "h", "i"))
			val builder = new SeqBuilder[Int, String, ExpectedPrimitive.type, Seq[String]](new PrimitiveSeqBuilder[String])
			val parser = new SeqParser[Int, String, Nothing, Seq[String]](new PrimitiveSeqParser[String])
			
			assertResult(Complex(exp)){parser.parse(builder, exp)}
		}
	}
}

