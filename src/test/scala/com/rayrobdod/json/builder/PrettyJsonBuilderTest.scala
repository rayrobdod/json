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

import java.text.ParseException;
import scala.collection.immutable.Map;
import org.scalatest.FunSpec;
import java.nio.charset.StandardCharsets.US_ASCII;
import java.nio.charset.StandardCharsets.UTF_8
import com.rayrobdod.json.union.JsonValue
import com.rayrobdod.json.union.JsonValue._
import com.rayrobdod.json.union.StringOrInt
import com.rayrobdod.json.union.StringOrInt._
import com.rayrobdod.json.parser.{IdentityParser, SeqParser, MapParser}
import com.rayrobdod.json.parser.{byteArray2DataInput, HexArrayStringConverter};

class PrettyJsonBuilderTest extends FunSpec {
	import PrettyJsonBuilder._
	
	def assertResultStr(e:String)(a:String):Unit = {
		def doReplace(s:String):String = {s.replace("\n", "\\n").replace("\t", "\\t").replace('[','(').replace(']',')')}
		
		assertResult(doReplace(e))(doReplace(a))
	}
	
	describe("PrettyJsonBuilderTest") {
		it ("Has correct init for tab-indent params") {
			val exp = "[\n\t\n]"
			val dut = new PrettyJsonBuilder(new IndentPrettyParams("\t", "\n"))
			assertResultStr(exp){dut.init}
		}
		it ("Has correct init for tab-indent, level 1 params") {
			val exp = "[\n\t\t\n\t]"
			val dut = new PrettyJsonBuilder(new IndentPrettyParams("\t", "\n"), level = 1)
			assertResultStr(exp){dut.init}
		}
		it ("Appends null to an array") {
			val exp = "[\n\tnull\n]"
			val dut = new PrettyJsonBuilder(new IndentPrettyParams("\t", "\n"))
			assertResultStr(exp){dut.apply(0, dut.init, JsonValueNull, new IdentityParser[StringOrInt,JsonValue])}
		}
		it ("Appends true to an array") {
			val exp = "[\n\ttrue\n]"
			val dut = new PrettyJsonBuilder(new IndentPrettyParams("\t", "\n"))
			assertResultStr(exp){dut.apply(0, dut.init, JsonValue(true), new IdentityParser[StringOrInt,JsonValue])}
		}
		it ("Appends a third value to an array") {
			val exp = "[\n\t1,\n\t2,\n\t3\n]"
			val dut = new PrettyJsonBuilder(new IndentPrettyParams("\t", "\n"))
			assertResultStr(exp){dut.apply(2, "[\n\t1,\n\t2\n]", JsonValue(3), new IdentityParser[StringOrInt,JsonValue])}
		}
		it ("Appends a third value to an object") {
			val exp = "{\n\t1,\n\t2,\n\t\"a\" : 3\n}"
			val dut = new PrettyJsonBuilder(new IndentPrettyParams("\t", "\n"))
			assertResultStr(exp){dut.apply("a", "{\n\t1,\n\t2\n]", JsonValue(3), new IdentityParser[StringOrInt,JsonValue])}
		}
		it ("Appends a third value, level 2") {
			val exp = "[\n\t\t1,\n\t\t2,\n\t\t3\n\t]"
			val dut = new PrettyJsonBuilder(new IndentPrettyParams("\t", "\n"), level = 1)
			assertResultStr(exp){dut.apply(2, "[\n\t\t1,\n\t\t2\n\t]", JsonValue(3), new IdentityParser[StringOrInt,JsonValue])}
		}
		it ("Appends an integer to an object") {
			val exp = """{\n\t"a" : 42\n}"""
			val dut = new PrettyJsonBuilder(new IndentPrettyParams("\t", "\n"))
			assertResultStr(exp){dut.apply("a", dut.init, JsonValue(42), new IdentityParser[StringOrInt,JsonValue])}
		}
		it ("can handle alternate tab strings") {
			val exp = """{\n    "a" : 42\n}"""
			val dut = new PrettyJsonBuilder(new IndentPrettyParams("    ", "\n"))
			assertResultStr(exp){dut.apply("a", dut.init, JsonValue(42), new IdentityParser[StringOrInt,JsonValue])}
		}
		it ("will throw if the folding string is not a valid json object or array (for certain easily detectable invalid values)") {
			val dut = new PrettyJsonBuilder(new IndentPrettyParams("\t", "\n"))
			intercept[IllegalArgumentException] {
				dut.apply("a", "", JsonValue(42), new IdentityParser[StringOrInt,JsonValue])
			}
		}
		it ("will throw if the folding is an object and the key is a number") {
			val dut = new PrettyJsonBuilder(MinifiedPrettyParams)
			intercept[IllegalArgumentException] {
				dut.apply(1, "{\"a\":\"b\"}", JsonValue(42), new IdentityParser[StringOrInt,JsonValue])
			}
		}
		it ("will throw if the folding is an array and the key is a string") {
			val dut = new PrettyJsonBuilder(MinifiedPrettyParams)
			intercept[IllegalArgumentException] {
				dut.apply("b", "[413]", JsonValue(42), new IdentityParser[StringOrInt,JsonValue])
			}
		}
		it ("will throw the first items key is a non-zero integer") {
			val dut = new PrettyJsonBuilder(MinifiedPrettyParams)
			intercept[IllegalArgumentException] {
				dut.apply(42, "[]", JsonValue(42), new IdentityParser[StringOrInt,JsonValue])
			}
		}
		it ("When the encoding parameter includes all characters of a string, it is included verbaitim") {
			assertResult("""{rest,"":"Pokémon"}"""){
				new PrettyJsonBuilder(MinifiedPrettyParams, UTF_8)
						.apply("", "{rest}", JsonValue("Pokémon"), new IdentityParser[StringOrInt,JsonValue])
			}
		}
		it ("When the encoding parameter does not include all characters of a string, the unknown characters are u-encoded") {
			assertResult("""{rest,"":"Pok\""" + """u00e9mon"}"""){
				new PrettyJsonBuilder(MinifiedPrettyParams, US_ASCII)
						.apply("", "{rest}", JsonValue("Pokémon"), new IdentityParser[StringOrInt,JsonValue])
			}
		}
		it ("throws on attempt to add a ByteStr to json object") {
			val dut = new PrettyJsonBuilder(MinifiedPrettyParams)
			intercept[UnsupportedOperationException] {
				dut.apply(1, "[413]", JsonValue(Array[Byte](1,2,3,4,5)), new IdentityParser[StringOrInt,JsonValue])
			}
		}
	}
	
	
	describe("PrettyJsonBuilderTest integration") {
		import com.rayrobdod.json.parser.JsonParser
		
		describe ("with JsonParser") {
			it ("will echo a properly formatted string") {
				val exp = "[\n\t61,\n\t62,\n\t63\n]"
				val dut = new PrettyJsonBuilder(new IndentPrettyParams("\t", "\n")).mapValue[JsonValue]
				assertResult(exp){new JsonParser().parse(dut, exp).left.get}
			}
			it ("will pretty-print a compacted string") {
				val exp = "[\n\t61,\n\t62,\n\t63\n]"
				val dut = new PrettyJsonBuilder(new IndentPrettyParams("\t", "\n")).mapValue[JsonValue]
				assertResult(exp){new JsonParser().parse(dut, "[61,62,63]").left.get}
			}
			it ("will minify with the MinifiedPrettyParams") {
				val exp = "[61,62,63]"
				val dut = new PrettyJsonBuilder(MinifiedPrettyParams).mapValue[JsonValue]
				assertResult(exp){new JsonParser().parse(dut, "[\n\t61,\n\t62,\n\t63\n]").left.get}
			}
			it ("will minify an object using MinifiedPrettyParams") {
				val exp = """{" a ":65}"""
				val dut = new PrettyJsonBuilder(MinifiedPrettyParams).mapValue[JsonValue]
				assertResult(exp){new JsonParser().parse(dut, " { \" a \" : 65 } ").left.get}
			}
			it ("nested arrays") {
				val exp = "[\n\t[\n\t\t0\n\t]\n]"
				val dut = new PrettyJsonBuilder(new IndentPrettyParams("\t", "\n")).mapValue[JsonValue]
				assertResult(exp){new JsonParser().parse(dut, "[[0]]").left.get}
			}
			it ("object inside array") {
				val exp = "[\n\t{\n\t\t\"value\" : 10\n\t},\n\t{\n\t\t\"value\" : 20\n\t}\n]"
				val dut = new PrettyJsonBuilder(new IndentPrettyParams("\t", "\n")).mapValue[JsonValue]
				assertResult(exp){new JsonParser().parse(dut, """[{"value":10},{"value":20}]""").left.get}
			}
		}
	}
}
