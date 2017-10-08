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
import com.rayrobdod.json.builder.{MapBuilder, PrettyJsonBuilder, ThrowBuilder}
import com.rayrobdod.json.union.{StringOrInt, JsonValue}
import com.rayrobdod.json.union.ParserRetVal.{Complex, BuilderFailure}
import com.rayrobdod.json.testing.EnforcedFailure

class MapParserTest extends FunSpec {
	describe("MapParser") {
		it ("""recreates an arbitrary map""") {
			val src = Map[String, Any]("a" -> 32, "b" -> Some(false), "c" -> MapBuilder.apply)
			val res = new MapParser().parse(MapBuilder[String,Any], src)
			
			assertResult(Complex(src.mapValues{Right.apply})){res}
		}
		it ("""recreates an arbitrary map with nesting""") {
			val exp = Complex(Map("a" -> Right(Map.empty), "b" -> Right(Map("x" -> true, "y" -> false))))
			val src = Map("a" -> Map.empty, "b" -> Map("x" -> true, "y" -> false))
			val res = new MapParser().parse(MapBuilder[String,Any], src)
			
			assertResult(exp){res}
		}
		it ("""builder failure""") {
			val exp = BuilderFailure(EnforcedFailure, ())
			val src = Map("a" -> Map.empty, "b" -> Map("x" -> true, "y" -> false))
			val res = new MapParser().parse(new ThrowBuilder(EnforcedFailure), src)
			
			assertResult(exp){res}
		}
	}
	
	describe("MapParser + Json") {
		val throwUnexpected = {x:Any => throw new NoSuchElementException(x.toString)}
		val throwUnexpected2 = {(x:Any, y:Unit) => throw new NoSuchElementException(x.toString)}
		
		it ("""can be used with the json stuff to serialze and deserialize a map""") {
			val src = Map(StringOrInt("a") -> JsonValue(32L), StringOrInt("b") -> JsonValue(false), StringOrInt("c") -> JsonValue("1.5"))
			val json = new MapParser().parse(new PrettyJsonBuilder(PrettyJsonBuilder.MinifiedPrettyParams), src)
					.fold({x => x}, throwUnexpected, throwUnexpected, throwUnexpected2)
			val res = new JsonParser().parse(MapBuilder[StringOrInt, JsonValue], json)
			
			assertResult(Complex(src.mapValues{Right.apply})){res}
		}
	}
}
