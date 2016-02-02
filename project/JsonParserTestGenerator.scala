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
import sbt._
import Keys._
import java.nio.charset.StandardCharsets.UTF_8

object JsonParserTestGenerator {
	
	private val testValues:Seq[(String, String, String)] = Seq(
		("empty array", """[]""", "Map.empty"),
		("empty array with before whitespace", """  []""", "Map.empty"),
		("empty array with before byteordermark", """\ufeff[]""", "Map.empty"),
		("empty array with middle whitespace", """[  ]""", "Map.empty"),
		("empty array with ending whitespace", """[]  """, "Map.empty"),
		("empty object", """{}""", "Map.empty"),
		("empty object with before byteordermark", """\ufeff{}""", "Map.empty"),
		("empty object with before whitespace", """  {}""", "Map.empty"),
		("empty object with middle whitespace", """{  }""", "Map.empty"),
		("empty object with ending whitespace", """{}  """, "Map.empty"),
		
		("array containing empty string", """[""]""", """Map(Right(0) -> "")"""),
		("array containing single char string", """["1"]""", """Map(Right(0) -> "1")"""),
		("array containing multi char string", """["abcde"]""", """Map(Right(0) -> "abcde")"""),
		("array containing multiple strings", """["a","b","c"]""", """Map(Right(0) -> "a", Right(1) -> "b", Right(2) -> "c")"""),
		
		("array containing positive number", """[24601]""", """Map(Right(0) -> 24601)"""),
		("array containing positive number (whitespace)", """[ 24601 ]""", """Map(Right(0) -> 24601)"""),
		("array containing zero", """[0]""", """Map(Right(0) -> 0)"""),
		("array containing negative number", """[-42]""", """Map(Right(0) -> -42)"""),
		("array containing multiple numbers", """[-1,0,1]""", """Map(Right(0) -> -1, Right(1) -> 0, Right(2) -> 1)"""),
		
		("array containing array", """[[[]]]""", """Map(Right(0) -> Map(Right(0) -> Map()))"""),
		("array containing array (whitespace)", """[ [ [ ] ] ]""", """Map(Right(0) -> Map(Right(0) -> Map()))"""),
		("array containing array 2", """[[0,1]]""", """Map(Right(0) -> Map(Right(0) -> 0, Right(1) -> 1))"""),
		("array containing object", """[{}]""", """Map(Right(0) -> Map())"""),
		
		("object containing empty string", """{"":""}""", """Map(Left("") -> "")"""),
		("object containing single char string", """{"a":"b"}""", """Map(Left("a") -> "b")"""),
		("object containing multi char string", """{"abcde":"12345"}""", """Map(Left("abcde") -> "12345")"""),
		("object containing multi char string (whitespace)", """{ "abcde" : "12345" }""", """Map(Left("abcde") -> "12345")"""),
		("object containing multiple string", """{"a":"b","c":"d"}""", """Map(Left("a") -> "b", Left("c") -> "d")"""),
		
		("object containing positive number", """{"":24601}""", """Map(Left("") -> 24601)"""),
		("object containing zero", """{"":0}""", """Map(Left("") -> 0)"""),
		("object containing negative number", """{"":-42}""", """Map(Left("") -> -42)"""),
		("object containing multiple numbers", """{"a":-1,"b":0,"c":1}""", """Map(Left("a") -> -1, Left("b") -> 0, Left("c") -> 1)"""),
		("object containing positive number (whitespace)", """{"": 24601 }""", """Map(Left("") -> 24601)"""),
		
		("object containing float", """{"": 1.2}""", """Map(Left("") -> 1.2)"""),
		// leading zeros are required?
		("object containing float (negative)", """{"": -1.2}""", """Map(Left("") -> -1.2)"""),
		("object containing float (exponent)", """{"": 1e5}""", """Map(Left("") -> 1e5)"""),
		("object containing float (decimal and exponent)", """{"": 1.2e5}""", """Map(Left("") -> 1.2e5)"""),
		("object containing float (decimal and neg exponent)", """{"": 1.2e-5}""", """Map(Left("") -> 1.2e-5)"""),
		
		("object containing array", """{"":[[]]}""", """Map(Left("") -> Map(Right(0) -> Map()))"""),
		("object containing array (whitespace)", """{"":[ [] ]}""", """Map(Left("") -> Map(Right(0) -> Map()))"""),
		("object containing array 2", """{"":[0,1]}""", """Map(Left("") -> Map(Right(0) -> 0, Right(1) -> 1))"""),
		("object containing object", """{"":{}}""", """Map(Left("") -> Map())"""),
		("object containing object (whitespace)", """{ "" : {} }""", """Map(Left("") -> Map())"""),
		
		("string containing escape codes (newline)", """["\n"]""", """Map(Right(0) -> "\n")"""),
		("string containing escape codes (tab)", """["\t"]""", """Map(Right(0) -> "\t")"""),
		("string containing escape codes (backslash)", """["\\"]""", """Map(Right(0) -> "\\")"""),
		("string containing escape codes (other)", """["\"\/\b\f\r"]""", """Map(Right(0) -> "\"/\b\f\r")"""),
		// that time where I have to perform a 'bobby tables' to force the thing to put unicode escapes into a JSON Parser 
		("string containing escape codes (unicode)", "[\"\\\"\"\" + \"\"\"u0123\"]", """Map(Right(0) -> "ģ")"""),
		("string containing escape codes (unicode) 2", "[\"\\\"\"\" + \"\"\"u221E\"]", """Map(Right(0) -> "\u221E")"""),
		("string containing escape codes (unicode) 3", "[\"\\\"\"\" + \"\"\"u0041A\"]", """Map(Right(0) -> "AA")"""),
		("array containing keyword (true)", """[true]""", """Map(Right(0) -> true)"""),
		("array containing keyword (false)", """[false]""", """Map(Right(0) -> false)"""),
		("array containing keyword (null)", """[null]""", """Map(Right(0) -> null)"""),
		("array containing keyword (null) (whitespace)", """[ null ]""", """Map(Right(0) -> null)""")
		
	)
	
	
	
	private val prefix:String = """
package com.rayrobdod.json.parser;

import java.text.ParseException;
import scala.collection.immutable.Map;
import org.scalatest.FunSpec;
import com.rayrobdod.json.builder.MapBuilder;
import com.rayrobdod.json.union.StringOrInt.{Left, Right}

class JsonParserTest_Happy extends FunSpec {
	describe("JsonParser + MapBuilder can decode") {"""
		
	private val suffix:String = "\n\t}\n}"
	
	private val testStrings:Seq[String] = testValues.toSeq.map{abc =>
		val (name:String, source:String, expected:String) = abc
		
		"\n\t\tit (\"\"\"" + name + "\"\"\"" + """) {
			val source = """ + "\"\"\"" + source + "\"\"\"" + """
			val expected = """ + expected + """
			val result = new JsonParser(new MapBuilder()).parse(source)
			assertResult(expected){result}
		}
		it (""" + '"' + name + """ (reader)") {
			val source = new java.io.StringReader(""" + "\"\"\"" + source + "\"\"\"" + """)
			val expected = """ + expected + """
			val result = new JsonParser(new MapBuilder()).parse(source)
			assertResult(expected){result}
		}"""
	}
	
	
	val generateJsonParserTest = TaskKey[Seq[File]]("generateJsonParserTest")
	
	
	val settings = Seq(
		generateJsonParserTest in Test := {
			val file = (resourceManaged in Test).value / "JsonParserTest_Happy.scala"
			sbt.IO.write(file, prefix, UTF_8, false)
			testStrings.foreach{test => sbt.IO.write(file, test, UTF_8, true)}
			sbt.IO.write(file, suffix, UTF_8, true)
			Seq(file)
		},
		sourceGenerators in Test <+= generateJsonParserTest in Test
	)
	
}
