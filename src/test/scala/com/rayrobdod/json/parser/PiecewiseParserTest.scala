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
import scala.collection.immutable.Map
import com.rayrobdod.json.builder.{Builder, MapBuilder, PrettyJsonBuilder}
import com.rayrobdod.json.builder.MapBuilder.{RecursiveSubject => MBRS}
import com.rayrobdod.json.union.JsonValue
import com.rayrobdod.json.union.ParserRetVal
import com.rayrobdod.json.union.ParserRetVal.{ParserFailure, Complex}
import com.rayrobdod.json.builder.PiecewiseBuilder.Failures
import com.rayrobdod.json.builder.PiecewiseBuilder.Failures.{ExpectedPrimitive, UnknownKey}

class PiecewiseParserTest extends FunSpec {
	private class Foo(val hello:Long, val world:String, val bazz:Boolean)
	private class Bar(val a:Foo, val b:Foo)
	import PiecewiseParser._
	
	describe("PiecewiseParser") {
		it ("when empty, does nothing") {
			val exp = Map()
			val src = new Foo(42, "addf", false)
			val res = new PiecewiseParser[String, JsonValue, Foo].parse(MapBuilder.apply, src)
			assertResult(Complex(exp)){res}
		}
		it ("""single keydef acts only on that keydef""") {
			val exp = Map("hello" -> Right(JsonValue(42)))
			val src = new Foo(hello = 42, world = "fsdf", bazz = false)
			val res = new PiecewiseParser[String, JsonValue, Foo](
					primitiveKeyDef("hello", (foo:Foo) => (JsonValue(foo.hello)))
				).parse(MapBuilder.apply, src)
			assertResult(Complex(exp)){res}
		}
		it ("""multiple keydefs act on each keydef""") {
			val exp = Map("hello" -> Right(JsonValue(12)), "world" -> Right(JsonValue("fasd")), "bazz" -> Right(JsonValue(true)))
			val src = new Foo(hello = 12, world = "fasd", bazz = true)
			val res = new PiecewiseParser[String, JsonValue, Foo](
					primitiveKeyDef("hello", {(foo:Foo) => (JsonValue(foo.hello))}),
					primitiveKeyDef("world", {(foo:Foo) => (JsonValue(foo.world))}),
					primitiveKeyDef("bazz", {(foo:Foo) => (JsonValue(foo.bazz))})
				).parse(MapBuilder.apply, src)
			assertResult(Complex(exp)){res}
		}
		it ("""nesting""") {
			val fooParser = new PiecewiseParser[String, JsonValue, Foo](
					primitiveKeyDef("h", {(foo:Foo) => (JsonValue(foo.hello))}),
					primitiveKeyDef("w", {(foo:Foo) => (JsonValue(foo.world))})
				)
			val barParser = new PiecewiseParser[String, JsonValue, Bar](
					complexKeyDef("a", {(bar:Bar) => bar.a}, fooParser),
					complexKeyDef("b", {(bar:Bar) => bar.b}, fooParser)
				)
			
			val exp = Map("a" -> Left(MBRS(Map("h" -> Right(JsonValue(1)), "w" -> Right(JsonValue("2"))))),
					"b" -> Left(MBRS(Map("h" -> Right(JsonValue(3)), "w" -> Right(JsonValue("4")))))
			)
			val src = new Bar(new Foo(1, "2", false), new Foo(3, "4", true))
			val res = barParser.parse(MapBuilder.apply, src)
			assertResult(Complex(exp)){res}
		}
	}
	
	
	describe ("Key defs") {
		describe("primitiveKeyDef") {
			it ("calls builder#apply using arguments obtained from backing") {
				val key = "key"
				val value = "value"
				
				val exp = Map(key -> Right(value))
				val dut = primitiveKeyDef(key = key, inputToValue = {x:String => value})
				val res = dut.apply[Nothing](MapBuilder.apply)("input", Map.empty)
				assertResult(Complex(exp)){res}
			}
		}
		describe("complexKeyDef") {
			it ("calls builder#apply using arguments obtained from backing") {
				type Key = String
				type Value = String
				type Input = String
				val key = "key"
				val value = "value"
				val child:Parser[Key, Value, Failures, Input] = new Parser[Key, Value, Failures, Input] {
					def parse[ComplexOutput, BF](builder:Builder[Key, Value, BF, ComplexOutput], i:Input):ParserRetVal[ComplexOutput, Value, Failures, BF] = ParserFailure(ExpectedPrimitive)
				}
				
				object EchoBuilder extends Builder[Key, Value, Nothing, (Key, Any, Parser[_,_,_,_])] {
					override type Middle = (Key, Any, Parser[_,_,_,_])
					def apply[Input,PF](folding:(Key, Any, Parser[_,_,_,_]), key:Key, input:Input, parser:Parser[Key,Value,PF,Input]):ParserRetVal[(Key, Any, Parser[_,_,_,_]), Nothing, PF, Nothing] = {
						Complex(((key, input, parser)))
					}
					def init:(Key, Any, Parser[_,_,_,_]) =  ("","",null)
					override def finish(x:Middle) = ParserRetVal.Complex(x)
				}
				
				val dut = complexKeyDef(key = key, backing = {x:String => value}, child)
				val res = dut.apply(EchoBuilder)("input", ("","",null))
				assertResult(Complex(key, value, child)){res}
			}
		}
		describe("optionalKeyDef") {
			it ("forwards to inner when filter is true") {
				val dut = optionalKeyDef(primitiveKeyDef(1, {x:Any => 1}), {x:Any => true})
				val res = dut.apply[Any](MapBuilder.apply)("input", Map.empty)
				assertResult(Complex(Map(1 -> Right(1)))){res}
			}
			it ("returns folding when filter is false") {
				val dut = optionalKeyDef(primitiveKeyDef(1, {x:Any => (1)}), {x:Any => false})
				val res = dut.apply[Any](MapBuilder.apply)("input", Map.empty)
				assertResult(Complex(Map())){res}
			}
		}
		
	}
	
	describe ("KeyDef syntax") {
		describe ("valueIs primitive") {
			it ("asdf") {
				val key = "key"
				val value = "value"
				
				val exp = Map(key -> Right(value))
				val dut:KeyDef[String, String, String] = key valueIs {x => value}
				val res = dut.apply[Nothing](MapBuilder.apply)("input", Map.empty)
				assertResult(Complex(exp)){res}
			}
		}
		describe("valueIs complex") {
			it ("asdf") {
				type Key = String
				type Value = String
				type Input = String
				val key = "key"
				val value = "value"
				val child = new Parser[Key, Value, Failures, Input] {
					def parse[ComplexOutput, BF](builder:Builder[Key, Value, BF, ComplexOutput], i:Input):ParserRetVal[ComplexOutput, Value, Failures, BF] = {
						ParserFailure(UnknownKey)
					}
				}
				
				object EchoBuilder extends Builder[Key, Value, Nothing, (Key, Any, Parser[_,_,_,_])] {
					type Middle = (Key, Any, Parser[_,_,_,_])
					def apply[Input, PF](folding:(Key, Any, Parser[_,_,_,_]), key:Key, input:Input, parser:Parser[Key,Value,PF,Input]):ParserRetVal[(Key, Any, Parser[_,_,_,_]), Nothing, PF, Nothing] = {
						Complex(((key, input, parser)))
					}
					def init:(Key, Any, Parser[_,_,_,_]) =  ("","",null)
					override def finish(x:Middle) = ParserRetVal.Complex(x)
				}
				
				val dut:KeyDef[Key, Value, Input] = key valueIs ({x:String => value}, child)
				val res = dut.apply(EchoBuilder)("input", ("","",null))
				assertResult(Complex( ((key, value, child)) )){res}
			}
		}
		describe ("valueIsOpt primitive") {
			it ("passes filter") {
				val exp = Map("key" -> Right("value"))
				val dut:KeyDef[String, String, Int] = "key" valueIsOpt {case x if x > 0 => "value"}
				val res = dut.apply[Nothing](MapBuilder.apply)(10, Map.empty)
				assertResult(Complex(exp)){res}
			}
			it ("fails filter") {
				val exp = Map()
				val dut:KeyDef[String, String, Int] = "key" valueIsOpt {case x if x > 0 => "value"}
				val res = dut.apply[Nothing](MapBuilder.apply)(-10, Map.empty)
				assertResult(Complex(exp)){res}
			}
		}
	}
	
	describe ("PiecewiseParser + JsonParser") {
		it ("documented example") {
			case class Foo(a:String, b:Seq[String], c:String)
			val nameParser = new PiecewiseParser[com.rayrobdod.json.union.StringOrInt, String, Foo](
				PiecewiseParser.primitiveKeyDef("a", {x => x.a}),
				PiecewiseParser.complexKeyDef("b", {x => x.b}, new PrimitiveSeqParser[String].mapKey[com.rayrobdod.json.union.StringOrInt]),
				PiecewiseParser.optionalKeyDef(PiecewiseParser.primitiveKeyDef("c", {x => x.c}), {x => x.c != ""}) 
			)
			val jsonBuilder = new PrettyJsonBuilder(PrettyJsonBuilder.MinifiedPrettyParams).mapValue[String]
			assertResult(Complex("""{"a":"","b":[]}""")){
				nameParser.parse(jsonBuilder, Foo("", Seq.empty, ""))
			}
			assertResult(Complex("""{"a":"qwer","b":["z","x","c"],"c":"asdf"}""")){
				nameParser.parse(jsonBuilder, Foo("qwer", Seq("z","x","c"), "asdf"))
			}
		}
	}
}
