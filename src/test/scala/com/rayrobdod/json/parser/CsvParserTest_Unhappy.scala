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
import com.rayrobdod.json.union.ParserRetVal
import com.rayrobdod.json.testing.EnforcedFailure
import com.rayrobdod.json.builder._

class CsvParserTest_Unhappy extends FunSpec {
	
	private val fieldThrowBuilder = MapBuilder[Int, String]
			.flatMapValue{x:String => if (x == "23") {Left(EnforcedFailure)} else {Right(x)}}
			.mapFailure{_.merge}
	private val recordThrowBuilder = SeqBuilder(
			  PrimitiveSeqBuilder[String, String]("inner")
				.flatMapResult{x:Seq[String] => if (x == Seq("A", "B", "C")) {Left(EnforcedFailure)} else {Right(x)}}
				.mapFailure{_.merge}
			, "outer")
	
	describe("CsvWithHeaderParser") {
		it ("When a builder for a field throws (0)") {
			val source = "23,b,c\nd,e,f\n"
			val exp = ParserRetVal.BuilderFailure(EnforcedFailure, CharacterIndex(0))
			assertResult(exp){new CsvParser().parse(fieldThrowBuilder, source)}
		}
		it ("When a builder for a field throws (2)") {
			val source = "a,23,c\nd,e,f\n"
			val exp = ParserRetVal.BuilderFailure(EnforcedFailure, CharacterIndex(2))
			assertResult(exp){new CsvParser().parse(fieldThrowBuilder, source)}
		}
		it ("When a builder for a field throws (8)") {
			val source = "a,b,c\nd,23,f\n"
			val exp = ParserRetVal.BuilderFailure(EnforcedFailure, CharacterIndex(8))
			assertResult(exp){new CsvParser().parse(fieldThrowBuilder, source)}
		}
		
		it ("When a builder for a record throws (0)") {
			val source = "A,B,C\nd,e,f\n"
			val exp = ParserRetVal.BuilderFailure(EnforcedFailure, CharacterIndex(0))
			assertResult(exp){new CsvParser().parse(recordThrowBuilder, source)}
		}
		it ("When a builder for a record throws (6)") {
			val source = "d,e,f\nA,B,C\n"
			val exp = ParserRetVal.BuilderFailure(EnforcedFailure, CharacterIndex(6))
			assertResult(exp){new CsvParser().parse(recordThrowBuilder, source)}
		}
	}
}
