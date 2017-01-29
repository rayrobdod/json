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

import scala.collection.immutable.Seq
import org.scalatest.FunSpec;
import com.rayrobdod.json.union.StringOrInt
import com.rayrobdod.json.union.JsonValue
import com.rayrobdod.json.union.ParserRetVal
import com.rayrobdod.json.builder.MapBuilder

class ShapelessParserTest extends FunSpec {
	case class Name(given:String, middle:String, family:String)
	case class IceCream(flavor:String, scoops:Int, waffleCone:Boolean)
	case class Record(holder:Name, description:String)
	
	describe("ShaplessParser") {
		describe("of Name to String") {
			import ShapelessParser._
			val parser = new ShapelessParser[String, Name]()
			
			it ("doThing") {
				val exp = Map("given" -> Right("a"), "middle" -> Right("b"), "family" -> Right("c"))
				val dut = Name("a", "b", "c")
				val res = parser.parse(MapBuilder.apply, dut)
				assertResult(ParserRetVal.Complex(exp)){res}
			}
		}
		describe("of Name to JsonValue") {
			import ShapelessParser._
			val parser = new ShapelessParser[JsonValue, Name]()
			
			it ("doThing") {
				val exp = Map("given" -> Right(JsonValue("a")), "middle" -> Right(JsonValue("b")), "family" -> Right(JsonValue("c")))
				val dut = Name("a", "b", "c")
				val res = parser.parse(MapBuilder.apply, dut)
				assertResult(ParserRetVal.Complex(exp)){res}
			}
		}
		describe("of IceCream to JsonValue") {
			import ShapelessParser._
			import JsonValue._
			val parser = new ShapelessParser[JsonValue, IceCream]()
			
			it ("doThing") {
				val exp = Map(
					"flavor" -> Right(JsonValue("Vanilla")),
					"scoops" -> Right(JsonValue(2)),
					"waffleCone" -> Right(JsonValue(false))
				)
				val dut = IceCream("Vanilla", 2, false)
				val res = parser.parse(MapBuilder.apply, dut)
				assertResult(ParserRetVal.Complex(exp)){res}
			}
		}
		describe("of Record to JsonValue") {
			import ShapelessParser._
			import JsonValue._
			import MapBuilder.RecursiveSubject
			implicit val nameParser = new ShapelessParser[JsonValue, Name]
			val parser = new ShapelessParser[JsonValue, Record]()
			
			it ("doThing") {
				val exp = Map(
					"description" -> Right(JsonValue("did good")),
					"holder" -> Left(RecursiveSubject(Map(
						"given" -> Right(JsonValue("Anon")),
						"middle" -> Right(JsonValue("Ee")),
						"family" -> Right(JsonValue("Mouse"))
					)))
				)
				val dut = Record(Name("Anon", "Ee", "Mouse"), "did good")
				val res = parser.parse(MapBuilder.apply, dut)
				assertResult(ParserRetVal.Complex(exp)){res}
			}
		}
	}
	
	describe("ShapelessParser integration") {
		import com.rayrobdod.json.builder.{PrettyJsonBuilder}
		import ShapelessParser._
		
		it ("ShapelessParser + JsonBuilder + primitives") {
			assertResult("""{"family":"Simpson","middle":"Jay","given":"Homer"}"""){
				new ShapelessParser[JsonValue, Name].mapKey[StringOrInt].parse(
					new PrettyJsonBuilder(PrettyJsonBuilder.MinifiedPrettyParams),
					Name("Homer", "Jay", "Simpson")
				).fold({x => x}, {x => x}, {(s,i) => ((s,i))})
			}
		}
	}
}
