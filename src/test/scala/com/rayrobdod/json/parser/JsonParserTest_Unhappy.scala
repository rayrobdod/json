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
package com.rayrobdod.json.parser;

import org.scalatest.FunSpec;
import com.rayrobdod.json.union.{JsonValue, StringOrInt, ParserRetVal, NonPrimitiveParserRetVal}
import com.rayrobdod.json.builder._

class JsonParserTest_Unhappy extends FunSpec {
	
	/**
	 * This is a wrapper required to put builders with different subjects in the same array.
	 * The only reason erasing the subject is remotely acceptable is because the tests assume a failure (and thus fail on a successful parse) 
	 */
	final class SubjectAsAnyBuilder[K,V,S](backing:Builder[K,V,S]) extends Builder[K,V,Any] {
		override def init:Any = backing.init
		override def apply[Input](folding:Any, key:K, input:Input, parser:Parser[K, V, Input]):NonPrimitiveParserRetVal[Any] = {
			val folding2:S = folding.asInstanceOf[S]
			backing.apply[Input](folding2, key, input, parser)
		}
	}
	
	private val parser = new JsonParser()
	private val mapBuilder = new SubjectAsAnyBuilder(MapBuilder[StringOrInt, JsonValue])
	private val seq2Builder = new SubjectAsAnyBuilder(new SeqBuilder(new PrimitiveSeqBuilder[JsonValue]))
	private val throwBuilder = new SubjectAsAnyBuilder(new ThrowBuilder[StringOrInt, JsonValue])
	
	private val failureCases:Seq[(String, Iterable[Char], Builder[StringOrInt, JsonValue, Any], Option[String], Option[Int])] = Seq(
		  ("errors when object is incomplete", """{""", mapBuilder, None, Option(1))
		, ("errors when array is incomplete", "[", mapBuilder, None, Option(1))
		, ("error idx includes insignificant whitespace", " [", mapBuilder, None, Option(2))
		, ("errors when string starts with non-space characters", "abc", mapBuilder, None, Option(0))
		, ("errors when map key is not a string", "{2:3}", mapBuilder, None, Option(1))
		, ("errors when map key is invalid string", """{"\u0001":3}""", mapBuilder, None, Option(2))
		, ("errors when map key is more than just a string", """{"fd" null}""", mapBuilder, None, Option(6))
		, ("errors when map value is more than just a string", """{" ":"hello" "world"}""", mapBuilder, None, Option(13))
		, ("errors when array value is more than just a string", """["hello" "world"]""", mapBuilder, None, Option(9))
		, ("errors when object value is not alphanumeric", """{"fd":%%%}""", mapBuilder, None, Option(6))
		, ("errors when array value is not alphanumeric", """[%%%]""", mapBuilder, None, Option(1))
		, ("errors when array value is not a keyword", """[nothing]""", mapBuilder, Some("Unexpected keyword: nothing"), Option(1))
		, ("errors when number starts with a decimal point (array)", """[.5]""", mapBuilder, Some("Numeric value may not begin with a '.'"), Option(1))
		, ("errors when number starts with a decimal point (object)", """{"":.5}""", mapBuilder, Some("Numeric value may not begin with a '.'"), Option(4))
		, ("errors when number starts with an exponent indicator (array)", """[e5]""", mapBuilder, Some("Unexpected keyword: e5"), Option(1))
		, ("errors when number starts with a exponent indicator (object)", """{"":e5}""", mapBuilder, Some("Unexpected keyword: e5"), Option(4))
		, ("errors when number starts with a plus sign (array)", """[+5]""", mapBuilder, Some("Expecting start of value; found +"), Option(1))
		, ("errors when number starts with a plus sign (object)", """{"":+5}""", mapBuilder, Some("Expecting start of value; found +"), Option(4))
		, ("number format (2)", """[51sfd]""", mapBuilder, Some("Not a number: 51sfd"), Option(1))
		, ("number format", """{"":51sfd}""", mapBuilder, Some("Not a number: 51sfd"), Option(4))
		, ("numbers cannot end with a dot", """{"":2.}""", mapBuilder, Some("Not a number: 2."), Option(4))
		, ("numbers cannot end with an e", """{"":2e}""", mapBuilder, Some("Not a number: 2e"), Option(4))
		, ("numbers cannot be just a hypen-minus", """{"":-}""", mapBuilder, Some("Not a number: -"), Option(4))
		, ("numbers must have digits between '.' and 'e'", """{"":9.e+1}""", mapBuilder, Some("Not a number: 9.e+1"), Option(4))
		, ("numbers cannot contain a leading zero", """{"":012}""", mapBuilder, Some("Not a number: 012"), Option(4))
		, ("numbers cannot contain a leading zero, even with a leading hyphen-minus", """{"":-012}""", mapBuilder, Some("Not a number: -012"), Option(4))
		, ("'True' is not a keyword", """{"":True}""", mapBuilder, Some("Expecting start of value; found T"), Option(4))
		, ("'NaN' is not a keyword", """{"":NaN}""", mapBuilder, Some("Expecting start of value; found N"), Option(4))
		
		, ("errors on illegal escape character inside string", """["\a"]""", mapBuilder, None, Option(3))
		, ("errors on illegal escape character inside string (obj)", """{"":"\a"}""", mapBuilder, None, Option(6))
		, ("errors on illegal character in unicode escape", "[\"\\u1y34\"]", mapBuilder, None, Option(3))
		, ("errors on illegal character in unicode escape 2", "[\"\\u1Y4\"]", mapBuilder, None, Option(3))
		, ("errors on illegal character in unicode escape 3", "[\"\\u1 4\"]", mapBuilder, None, Option(3))
		, ("errors on illegal character in unicode escape 4", "[\"\\u1=4\"]", mapBuilder, None, Option(3))
		, ("errors on infinitely nested arrays", Seq.fill(1000)('[') ++ Seq.fill(1000)(']'), mapBuilder, Option("too-deeply nested object"), None)
		
		, ("errors on trailing comma (array)", """[1,2,3,]""", mapBuilder, None, Option(7))
		, ("errors on empty value (array)", """[1,,3]""", mapBuilder, None, Option(3))
		, ("errors on empty value 2 (array)", """[,]""", mapBuilder, None, Option(1))
		, ("errors on trailing comma (object)", """{"a":2,}""", mapBuilder, None, Option(7))
		, ("errors on empty value (object)", """{"":0,,}""", mapBuilder, None, Option(6))
		, ("errors on empty value 2 (object)", """{,}""", mapBuilder, None, Option(1))
		
		, ("provides a correct index in a nested value (array, array)", """[[,]]""", seq2Builder, None, Option(2))
		, ("provides a correct index in a nested value (array, object)", """[{},{,}]""", seq2Builder, None, Option(5))
		, ("provides a correct index in a nested value (object, array)", """{"":[,]}""", seq2Builder, None, Option(5))
		, ("provides a correct index in a nested value (object, object)", """{"" : {,}}""", seq2Builder, None, Option(7))
		
		, ("Throwbuilder (array of string)", """["ab"]""", throwBuilder, None, Option(1))
		, ("Throwbuilder (array of int -2)", """[-2]""", throwBuilder, None, Option(1))
		, ("Throwbuilder (array of int 5.5)", """[5.5]""", throwBuilder, None, Option(1))
		, ("Throwbuilder (array of true)", """[true]""", throwBuilder, None, Option(1))
		, ("Throwbuilder (object of string)", """{"":"ab"}""", throwBuilder, None, Option(4))
		, ("Throwbuilder (object of int -2)", """{"":-2}""", throwBuilder, None, Option(4))
		, ("Throwbuilder (object of int 5.5)", """{"":5.5}""", throwBuilder, None, Option(4))
		, ("Throwbuilder (object of true)", """{"":true}""", throwBuilder, None, Option(4))
	)
	
	
	describe("JsonParser") {
		failureCases.foreach{abcde =>
			val (name, source, builder, expectedMsg, expectedIdx) = abcde
			
			it (name) {
				parser.parse(builder, source) match {
					case ParserRetVal.Failure(msg, idx) => {
						expectedMsg.foreach{x => assertResult(x){msg}}
						expectedIdx.foreach{x => assertResult(x){idx}}
					}
					case x => fail(s"Not a failure: $x")
				}
			}
		}
		
		it ("""errors on control character inside string""") {
			('\u0000' until ' ').foreach{c =>
				val source = Seq('[', '"', c, '"', ']')
				
				parser.parse(mapBuilder, source) match {
					case ParserRetVal.Failure(msg, idx) => {
						assertResult("Control chars not allowed in strings"){msg}
						assertResult(2){idx}
					}
					case x => fail(s"Not a failure: $x")
				}
			}
		}
	}
}
