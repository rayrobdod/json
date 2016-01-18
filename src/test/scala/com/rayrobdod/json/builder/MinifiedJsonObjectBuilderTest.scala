/*
	Copyright (c) 2015, Raymond Dodge
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

import scala.beans.BeanProperty;
import java.text.ParseException;
import scala.collection.immutable.Map;
import org.scalatest.FunSpec;
import java.nio.charset.StandardCharsets.US_ASCII;
import com.rayrobdod.json.parser.{byteArray2DataInput, HexArrayStringConverter}

class MinifiedJsonObjectBuilderTest extends FunSpec {
	private case class Abc(a:Int, b:Boolean, c:String)
	private implicit def classAbc = classOf[Abc]
	
	describe("MinifiedJsonObjectBuilder") {
		it ("inits correctly") {
			assertResult("{}"){new MinifiedJsonObjectBuilder().init}
		}
		it ("Appends null") {
			assertResult("""{"":null}"""){
				new MinifiedJsonObjectBuilder().apply("{}", "", null)
			}
		}
		it ("Appends true") {
			assertResult("""{"":true}"""){
				new MinifiedJsonObjectBuilder().apply("{}", "", true)
			}
		}
		it ("Appends false") {
			assertResult("""{"":false}"""){
				new MinifiedJsonObjectBuilder().apply("{}", "", false)
			}
		}
		it ("Appends integer") {
			assertResult("""{"":68}"""){
				new MinifiedJsonObjectBuilder().apply("{}", "", 68)
			}
		}
		it ("Appends seq") {
			assertResult("""{"":[1,2,3]}"""){
				new MinifiedJsonObjectBuilder().apply("{}", "", Seq(1,2,3))
			}
		}
		it ("Appends string") {
			assertResult("""{"":"abc"}"""){
				new MinifiedJsonObjectBuilder().apply("{}", "", "abc")
			}
		}
		it ("Appends string with escapes") {
			assertResult("""{"":"a\tc"}"""){
				new MinifiedJsonObjectBuilder().apply("{}", "", "a\tc")
			}
		}
		it ("Appends string with escapes 2") {
			assertResult("""{"":"a\""" + """u0000c"}"""){
				new MinifiedJsonObjectBuilder().apply("{}", "", "a\u0000c")
			}
		}
		it ("Appends string with space") {
			assertResult("""{"":" a c "}"""){
				new MinifiedJsonObjectBuilder().apply("{}", "", " a c ")
			}
		}
		it ("Appends string with non-ascii char (utf-8)") {
			assertResult("""{"":"Pokémon"}"""){
				new MinifiedJsonObjectBuilder().apply("{}", "", "Pokémon")
			}
		}
		it ("Appends string with non-ascii char (ascii)") {
			assertResult("""{"":"Pok\""" + """u00e9mon"}"""){
				new MinifiedJsonObjectBuilder(US_ASCII).apply("{}", "", "Pokémon")
			}
		}
		it ("Appends a second value") {
			assertResult("""{"a":"b","c":"d"}"""){
				new MinifiedJsonObjectBuilder().apply("""{"a":"b"}""", "c", "d")
			}
		}
		it ("childBuilder returns MapBuilder") {
			val builder = new MinifiedJsonObjectBuilder()
			
			assert{builder.childBuilder("").isInstanceOf[MapBuilder[_]]}
		}
		it ("resultType returns constructor parameter `clazz`") {
			assertResult(classOf[String]){
				new MinifiedJsonObjectBuilder().resultType
			}
		}
	}
	
	describe("MinifiedJsonObjectBuilder integration") {
		import com.rayrobdod.json.parser.{JsonParser, CborParser, CaseClassParser, MapParser}
		
		it ("MinifiedJsonObjectBuilder + JsonParser + primitive") {
			assertResult("""{"a":61,"b":62,"c":63}"""){
				new JsonParser(new MinifiedJsonObjectBuilder).parse(
					"""{"a":61,"b":62,"c":63}"""
				)
			}
		}
		it ("MinifiedJsonObjectBuilder + JsonParser + primitive (whitespace)") {
			assertResult("""{"a":61,"b":62,"c":63}"""){
				new JsonParser(new MinifiedJsonObjectBuilder).parse(
					"""{
	"a" : 61,
	"b" : 62,
	"c" : 63
}"""
				)
			}
		}
		it ("MinifiedJsonObjectBuilder + JsonParser + nested objects") {
			assertResult("""{"":{"a":0,"b":1}}"""){
				new JsonParser(new MinifiedJsonObjectBuilder).parse(
					"""{"":{"a":0,"b":1}}"""
				)
			}
		}
		it ("MinifiedJsonObjectBuilder + CborParser + primitives") {
			assertResult("""{"4":5}"""){
				new CborParser(new MinifiedJsonObjectBuilder).parse(
					byteArray2DataInput(hexArray"A10405")
				)
			}
		}
		it ("MinifiedJsonObjectBuilder + case class") {
			assertResult("""{"a":5,"b":false,"c":"str"}"""){
				new CaseClassParser(new MinifiedJsonObjectBuilder).parse(Abc(5,false,"str"))
			}
		}
		it ("MinifiedJsonObjectBuilder + nested case class") {
			val pf:PartialFunction[Any,Any] = {case x:Abc => new CaseClassParser(new MapBuilder).parse(x)}
			
			assertResult("""{"5":{"a":5,"b":false,"c":"str"}}"""){
				new MapParser(new MinifiedJsonObjectBuilder(transformer = pf)).parse(Map("5" -> Abc(5,false,"str")))
			}
		}
	}
}

