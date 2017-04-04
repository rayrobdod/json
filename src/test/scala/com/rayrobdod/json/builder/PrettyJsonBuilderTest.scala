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
import java.nio.charset.StandardCharsets.US_ASCII;
import java.nio.charset.StandardCharsets.UTF_8
import com.rayrobdod.json.testing.EnforcedFailure
import com.rayrobdod.json.union.StringOrInt
import com.rayrobdod.json.union.JsonValue
import com.rayrobdod.json.union.JsonValue._
import com.rayrobdod.json.union.ParserRetVal
import com.rayrobdod.json.union.ParserRetVal.{Complex, ParserFailure, BuilderFailure}
import com.rayrobdod.json.parser.{IdentityParser, FailureParser, JsonParser}

class PrettyJsonBuilderTest extends FunSpec {
	import PrettyJsonBuilder._
	import PrettyJsonBuilder.Failures._
	
	def countingReader(str:String) = new com.rayrobdod.json.parser.CountingReader(new java.io.StringReader(str))
	
	/** replaces characters that are special to both json and scalatest with characters not important to either */
	def assertResultStr(e:String)(a:String):Unit = {
		def doReplace(s:String):String = {s.replace("\n", "\\n").replace("\t", "\\t").replace('[','(').replace(']',')')}
		assertResult(doReplace(e))(doReplace(a))
	}
	/** replaces characters that are special to both json and scalatest with characters not important to either */
	def assertResultComplexStr(e:Complex[String])(a:ParserRetVal[String, _, _, _, _]):Unit = {
		val e2 = e.x
		a match {
			case Complex(a2) => assertResultStr(e2){a2}
			case x => fail("Expected Complex(string); was " + x)
		}
	}
	
	describe("PrettyJsonBuilderTest") {
		describe("init") {
			it ("returns the expected value") {
				val exp = new Middle(false, 0, Nil)
				val result = new PrettyJsonBuilder(new IndentPrettyParams("\t", "\n")).init
				assertResult(exp)(result)
			}
			it ("returns the expected value, regardless of prettyparams") {
				val exp = new Middle(false, 0, Nil)
				val result = new PrettyJsonBuilder(new IndentPrettyParams("  ", "  ")).init
				assertResult(exp)(result)
			}
		}
		describe("apply") {
			it ("Appends null to an array") {
				val exp = Complex(new Middle(false, 1, "null" :: Nil))
				val dut = new PrettyJsonBuilder(new IndentPrettyParams("\t", "\n"))
				assertResult(exp){dut.apply(dut.init, 0,JsonValueNull, new IdentityParser[JsonValue], ())}
			}
			it ("Appends true to an array") {
				val exp = Complex(new Middle(false, 1, "true" :: Nil))
				val dut = new PrettyJsonBuilder(new IndentPrettyParams("\t", "\n"))
				assertResult(exp){dut.apply(dut.init, 0,JsonValue(true), new IdentityParser[JsonValue], ())}
			}
			it ("Appends a third value to an array") {
				val exp = Complex(new Middle(false, 3, "3" :: "2" :: "1" :: Nil))
				val dut = new PrettyJsonBuilder(new IndentPrettyParams("\t", "\n"))
				assertResult(exp){dut.apply(new Middle(false, 2, "2" :: "1" :: Nil), 2,JsonValue(3), new IdentityParser[JsonValue], ())}
			}
			it ("Appends a third value to an object") {
				val exp = Complex(new Middle(true, 3, "\"a\" : 3" :: "2" :: "1" :: Nil))
				val dut = new PrettyJsonBuilder(new IndentPrettyParams("\t", "\n"))
				assertResult(exp){dut.apply(new Middle(true, 2, "2" :: "1" :: Nil), "a",JsonValue(3), new IdentityParser[JsonValue], ())}
			}
			it ("Appends an integer to an object") {
				val exp = Complex(new Middle(true, 1, "\"a\" : 42" :: Nil))
				val dut = new PrettyJsonBuilder(new IndentPrettyParams("\t", "\n"))
				assertResult(exp){dut.apply(dut.init, "a",JsonValue(42), new IdentityParser[JsonValue], ())}
			}
			it ("Appends an integer to an object (Miniparams)") {
				val exp = Complex(new Middle(true, 1, "\"a\":42" :: Nil))
				val dut = new PrettyJsonBuilder(MinifiedPrettyParams)
				assertResult(exp){dut.apply(dut.init, "a",JsonValue(42), new IdentityParser[JsonValue], ())}
			}
			it ("will throw if the folding is an object and the key is a number") {
				val dut = new PrettyJsonBuilder(MinifiedPrettyParams)
				assertResult(BuilderFailure(KeyTypeChangedMidObject(StringOrInt.Right(1), KeyTypeChangedMidObject.ExpectingString), ())){
					dut.apply(new Middle(true, 1, """ "a" : "b" """ :: Nil), 1,JsonValue(42), new IdentityParser[JsonValue], ())
				}
			}
			it ("will throw if the folding is an array and the key is a string") {
				val dut = new PrettyJsonBuilder(MinifiedPrettyParams)
				assertResult(BuilderFailure(KeyTypeChangedMidObject(StringOrInt.Left("b"), KeyTypeChangedMidObject.ExpectingInt), ())){
					dut.apply(new Middle(false, 1, "413" :: Nil), "b",JsonValue(42), new IdentityParser[JsonValue], ())
				}
			}
			it ("will throw the first items key is a non-zero integer") {
				val dut = new PrettyJsonBuilder(MinifiedPrettyParams)
				assertResult(BuilderFailure(ArrayKeyNotIncrementing(42,0), ())){
					dut.apply(dut.init, 42,JsonValue(42), new IdentityParser[JsonValue], ())
				}
			}
			it ("When the encoding parameter includes all characters of a string, it is included verbaitim") {
				assertResult(Complex(new Middle(true, 2, "\"\":\"Pokémon\"" :: "rest" :: Nil))){
					new PrettyJsonBuilder(MinifiedPrettyParams, UTF_8)
							.apply(new Middle(true, 1, "rest" :: Nil), "",JsonValue("Pokémon"), new IdentityParser[JsonValue], ())
				}
			}
			it ("When the encoding parameter does not include all characters of a string, the unknown characters are u-encoded") {
				assertResult(Complex(new Middle(true, 2, ("\"\":\"Pok\\" + "u00e9mon\"") :: "rest" :: Nil))){
					new PrettyJsonBuilder(MinifiedPrettyParams, US_ASCII)
							.apply(new Middle(true, 1, "rest" :: Nil), "",JsonValue("Pokémon"), new IdentityParser[JsonValue], ())
				}
			}
			it ("When parser reports a failure, the failure is forwarded") {
				assertResult( ParserFailure(EnforcedFailure) ){
					new PrettyJsonBuilder(MinifiedPrettyParams, UTF_8)
							.apply(new Middle(true, 1, "rest" :: Nil), "", "", new FailureParser(EnforcedFailure), ())
				}
			}
			it ("Inner Object") {
				val exp = Complex(new Middle(false, 1, "[\n\t\t1,\n\t\t2,\n\t\t3\n\t]" :: Nil))
				val builder = new PrettyJsonBuilder(new IndentPrettyParams("\t", "\n"))
				val result = builder.apply(builder.init, 0, countingReader("[1,2,3]"), new JsonParser, ())
				assertResult(exp){result}
			}
		}
		describe("finish") {
			it ("init (tab params)") {
				val exp = Complex("[\n\t\n]")
				val dut = new PrettyJsonBuilder(new IndentPrettyParams("\t", "\n"))
				assertResultComplexStr(exp)(dut.finish(())(dut.init))
			}
			it ("init (mini params)") {
				val exp = Complex("[]")
				val dut = new PrettyJsonBuilder(MinifiedPrettyParams)
				assertResultComplexStr(exp)(dut.finish(())(dut.init))
			}
			it ("one item (mini params) (array)") {
				val exp = Complex("[123]")
				val builder = new PrettyJsonBuilder(MinifiedPrettyParams)
				val middle = Middle(false, 1, "123" :: Nil)
				assertResultComplexStr(exp)(builder.finish(())(middle))
			}
			it ("one item (tab params) (array)") {
				val exp = Complex("[\n\t123\n]")
				val builder = new PrettyJsonBuilder(new IndentPrettyParams("\t", "\n"))
				val middle = Middle(false, 1, "123" :: Nil)
				assertResultComplexStr(exp)(builder.finish(())(middle))
			}
			it ("one item (tab params) (object)") {
				val exp = Complex("{\n\t123\n}")
				val builder = new PrettyJsonBuilder(new IndentPrettyParams("\t", "\n"))
				val middle = Middle(true, 1, "123" :: Nil)
				assertResultComplexStr(exp)(builder.finish(())(middle))
			}
			it ("two items (tab params) (array)") {
				val exp = Complex("[\n\t123,\n\t456\n]")
				val builder = new PrettyJsonBuilder(new IndentPrettyParams("\t", "\n"))
				val middle = Middle(false, 1, "456" :: "123" :: Nil)
				assertResultComplexStr(exp)(builder.finish(())(middle))
			}
			it ("two items (tab params) (array) (level 1)") {
				val exp = Complex("[\n\t\t123,\n\t\t456\n\t]")
				val builder = new PrettyJsonBuilder(new IndentPrettyParams("\t", "\n"), level = 1)
				val middle = Middle(false, 1, "456" :: "123" :: Nil)
				assertResultComplexStr(exp)(builder.finish(())(middle))
			}
			it ("two items (tab params) (array) (level 2)") {
				val exp = Complex("[\n\t\t\t123,\n\t\t\t456\n\t\t]")
				val builder = new PrettyJsonBuilder(new IndentPrettyParams("\t", "\n"), level = 2)
				val middle = Middle(false, 1, "456" :: "123" :: Nil)
				assertResultComplexStr(exp)(builder.finish(())(middle))
			}
			it ("two items (space2 params) (array) (level 1)") {
				val exp = Complex("[\n    {},\n    []\n  ]")
				val builder = new PrettyJsonBuilder(new IndentPrettyParams("  ", "\n"), level = 1)
				val middle = Middle(false, 1, "[]" :: "{}" :: Nil)
				assertResultComplexStr(exp)(builder.finish(())(middle))
			}
		}
	}
	describe("PrettyJsonBuilderTest$") {
		val thingToParse = """[true, 23, {"a":"b"}]"""
		it ("minified") {
			val b1 = PrettyJsonBuilder.minified()
			val b2 = new PrettyJsonBuilder(MinifiedPrettyParams)
			val r1 = new JsonParser().parse(b1, thingToParse)
			val r2 = new JsonParser().parse(b2, thingToParse)
			assertResult(r2)(r1)
		}
		it ("space2") {
			val b1 = PrettyJsonBuilder.space2()
			val b2 = new PrettyJsonBuilder(new IndentPrettyParams("  "))
			val r1 = new JsonParser().parse(b1, thingToParse)
			val r2 = new JsonParser().parse(b2, thingToParse)
			assertResult(r2)(r1)
		}
		it ("space4") {
			val b1 = PrettyJsonBuilder.space4()
			val b2 = new PrettyJsonBuilder(new IndentPrettyParams("    "))
			val r1 = new JsonParser().parse(b1, thingToParse)
			val r2 = new JsonParser().parse(b2, thingToParse)
			assertResult(r2)(r1)
		}
		it ("tabbed") {
			val b1 = PrettyJsonBuilder.tabbed()
			val b2 = new PrettyJsonBuilder(new IndentPrettyParams("\t"))
			val r1 = new JsonParser().parse(b1, thingToParse)
			val r2 = new JsonParser().parse(b2, thingToParse)
			assertResult(r2)(r1)
		}
	}
	
	
	describe("PrettyJsonBuilderTest integration") {
		import com.rayrobdod.json.parser.JsonParser
		
		describe ("with JsonParser") {
			it ("will echo a properly formatted string") {
				val exp = "[\n\t61,\n\t62,\n\t63\n]"
				val dut = new PrettyJsonBuilder(new IndentPrettyParams("\t", "\n")).mapValue[JsonValue]
				assertResult(Complex(exp)){new JsonParser().parse(dut, exp)}
			}
			it ("will pretty-print a compacted string") {
				val exp = "[\n\t61,\n\t62,\n\t63\n]"
				val dut = new PrettyJsonBuilder(new IndentPrettyParams("\t", "\n")).mapValue[JsonValue]
				assertResult(Complex(exp)){new JsonParser().parse(dut, "[61,62,63]")}
			}
			it ("will minify with the MinifiedPrettyParams") {
				val exp = "[61,62,63]"
				val dut = new PrettyJsonBuilder(MinifiedPrettyParams).mapValue[JsonValue]
				assertResult(Complex(exp)){new JsonParser().parse(dut, "[\n\t61,\n\t62,\n\t63\n]")}
			}
			it ("will minify an object using MinifiedPrettyParams") {
				val exp = """{" a ":65}"""
				val dut = new PrettyJsonBuilder(MinifiedPrettyParams).mapValue[JsonValue]
				assertResult(Complex(exp)){new JsonParser().parse(dut, " { \" a \" : 65 } ")}
			}
			it ("nested arrays") {
				val exp = "[\n\t[\n\t\t0\n\t]\n]"
				val dut = new PrettyJsonBuilder(new IndentPrettyParams("\t", "\n")).mapValue[JsonValue]
				assertResult(Complex(exp)){new JsonParser().parse(dut, "[[0]]")}
			}
			it ("object inside array") {
				val exp = "[\n\t{\n\t\t\"value\" : 10\n\t},\n\t{\n\t\t\"value\" : 20\n\t}\n]"
				val dut = new PrettyJsonBuilder(new IndentPrettyParams("\t", "\n")).mapValue[JsonValue]
				assertResult(Complex(exp)){new JsonParser().parse(dut, """[{"value":10},{"value":20}]""")}
			}
		}
	}
}
