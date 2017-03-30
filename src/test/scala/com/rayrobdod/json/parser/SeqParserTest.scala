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
import scala.collection.immutable.Seq
import com.rayrobdod.json.builder.{SeqBuilder, PrettyJsonBuilder, ThrowBuilder, PrimitiveSeqBuilder}
import com.rayrobdod.json.builder.PiecewiseBuilder.Failures.ExpectedComplex
import com.rayrobdod.json.builder.BuilderTest.EnforcedFailure
import com.rayrobdod.json.union.JsonValue
import com.rayrobdod.json.union.ParserRetVal.BuilderFailure

class SeqParserTest extends FunSpec {
	describe("SeqParser") {
		it ("""builder failure""") {
			val exp = BuilderFailure(EnforcedFailure)
			val src = Seq(Seq.empty, Seq(true, false))
			val res = new SeqParser(new PrimitiveSeqParser[Boolean]).parse(new ThrowBuilder(EnforcedFailure), src)
			
			assertResult(exp){res}
		}
	}
	
	describe("SeqParser + Json") {
		val throwUnexpected = {x:Any => throw new NoSuchElementException(x.toString)}
		
		it ("""can be used with the json stuff to serialze and deserialize a Seq""") {
			val src = Seq(Seq.empty, Seq(JsonValue(true), JsonValue(12.5)))
			val json = new SeqParser(new PrimitiveSeqParser[JsonValue])
					.parse(new PrettyJsonBuilder(PrettyJsonBuilder.MinifiedPrettyParams).mapKey[Int], src)
					.fold({x => x}, throwUnexpected, throwUnexpected, throwUnexpected)
			val res = new JsonParser()
					.parse(new SeqBuilder(PrimitiveSeqBuilder.apply[JsonValue], ExpectedComplex), json)
					.fold({x => x}, throwUnexpected, throwUnexpected, throwUnexpected)
			
			assertResult(src){res}
		}
	}
}
