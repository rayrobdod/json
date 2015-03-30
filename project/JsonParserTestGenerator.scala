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
		("empty array with middle whitespace", """[  ]""", "Map.empty"),
		("empty array with ending whitespace", """[]  """, "Map.empty"),
		("empty object", """{}""", "Map.empty"),
		("empty object with before whitespace", """  {}""", "Map.empty"),
		("empty object with middle whitespace", """{  }""", "Map.empty"),
		("empty object with ending whitespace", """{}  """, "Map.empty"),
		
		("array containing empty string", """[""]""", """Map("0" -> "")"""),
		("array containing single char string", """["1"]""", """Map("0" -> "1")"""),
		("array containing multi char string", """["abcde"]""", """Map("0" -> "abcde")"""),
		("array containing multiple strings", """["a","b","c"]""", """Map("0" -> "a", "1" -> "b", "2" -> "c")"""),
		
		("array containing positive number", """[24601]""", """Map("0" -> 24601)"""),
		("array containing zero", """[0]""", """Map("0" -> 0)"""),
		("array containing negative number", """[-42]""", """Map("0" -> -42)"""),
		("array containing multiple numbers", """[-1,0,1]""", """Map("0" -> -1, "1" -> 0, "2" -> 1)"""),
		
		("array containing array", """[[[]]]""", """Map("0" -> Map("0" -> Map()))"""),
		("array containing array 2", """[[0,1]]""", """Map("0" -> Map("0" -> 0, "1" -> 1))"""),
		("array containing object", """[{}]""", """Map("0" -> Map())"""),
		
		("object containing empty string", """{"":""}""", """Map("" -> "")"""),
		("object containing single char string", """{"a":"b"}""", """Map("a" -> "b")"""),
		("object containing multi char string", """{"abcde":"12345"}""", """Map("abcde" -> "12345")"""),
		("object containing multiple string", """{"a":"b","c":"d"}""", """Map("a" -> "b", "c" -> "d")"""),
		
		("object containing positive number", """{"":24601}""", """Map("" -> 24601)"""),
		("object containing zero", """{"":0}""", """Map("" -> 0)"""),
		("object containing negative number", """{"":-42}""", """Map("" -> -42)"""),
		("object containing multiple numbers", """{"a":-1,"b":0,"c":1}""", """Map("a" -> -1, "b" -> 0, "c" -> 1)"""),
		
		("object containing array", """{"":[[]]}""", """Map("" -> Map("0" -> Map()))"""),
		("object containing array 2", """{"":[0,1]}""", """Map("" -> Map("0" -> 0, "1" -> 1))"""),
		("object containing object", """{"":{}}""", """Map("" -> Map())"""),
		
		("string containing escape codes (newline)", """["\n"]""", """Map("0" -> "\n")"""),
		("string containing escape codes (tab)", """["\t"]""", """Map("0" -> "\t")"""),
		("string containing escape codes (backslash)", """["\\"]""", """Map("0" -> "\\")"""),
		// that time where I have to perform a 'bobby tables' to force the thing to put unicode escapes into a JSON Parser 
		("string containing escape codes (unicode)", "[\"\\\"\"\" + \"\"\"u0123\"]", """Map("0" -> "ģ")"""),
		("string containing escape codes (unicode) 2", "[\"\\\"\"\" + \"\"\"u221E\"]", """Map("0" -> "\u221E")"""),
		("string containing escape codes (unicode) 3", "[\"\\\"\"\" + \"\"\"u0041A\"]", """Map("0" -> "AA")"""),
		("array containing keyword (true)", """[true]""", """Map("0" -> true)"""),
		("array containing keyword (false)", """[false]""", """Map("0" -> false)"""),
		("array containing keyword (null)", """[null]""", """Map("0" -> null)""")
		
	)
	
	
	
	private val prefix:String = """
package com.rayrobdod.json.parser;

import java.text.ParseException;
import scala.collection.immutable.Map;
import org.scalatest.FunSpec;
import com.rayrobdod.json.builder.MapBuilder;

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
