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

object YamlParserTestGenerator {
	
	private val testValues:Seq[(String, String, String)] = Seq(
		("empty document", "", """ "" """),
		("comment", "# this is a comment", """ "" """),
		
		// scalar
//		("string (unquoted)", "abc", "\"abc\""),
		("string (quoted double)", "\"def\"", "\"def\""),
		("string (quoted single)", "'ghi'", "\"ghi\""),
/*		("string (block)",  """|
  \//||\/||
  // ||  ||__
""", "\"\"\"" + """\//||\/||
// ||  ||__
""" + "\"\"\""),
		("string (folded)", ">\n  how  many roads\n  must one  walk?", "\"how  many reoads must one  walk?\""),
*/		
		("string (double escape)", """ "l\"r" """, """ "l\"r" """),
		("string (single escape)", " 'l''r' ", """ "l'r" """),
		// TODO: indentation detection
		// TODO: indentation indicator
		// TODO: chomping indicator
		// TODO: line breaks inside double-quoted lines
		
		/* use failsafe schema, rather than json schema? */
/*		("integer (decimal)", """0""", "0"),
		("integer (hex)", """0xC""", "12"),
		("integer (octal)", """0o14""", "12"),
		("fractional (direct)", """1230.15""", "1230.15"),
		("fractional (exponental)", """12.3015e+02""", "1230.15"),
		("fractional (neginf)", "-.inf", "Double.NegativeInfinity"),
		("fractional (nan)", ".NaN", "Double.NaN"),
		("boolean (true)", "true", "true"),
		("boolean (false)", "false", "false"),
*/		
		// array
/*		("array (flow)", "[a,b]", """Map(0 -> "a", 1 -> "b")"""),
		("array (flow trailing comma)", "[a,b,]", """Map(0 -> "a", 1 -> "b")"""),
		("array (block)", "- a\n- b\n", """Map(0 -> "a", 1 -> "b")"""),
		("array (nested)", "- - a\n  - b\n- - c\n  - d\n", """Map(0 -> Map(0 -> "a", 1 -> "b"), 1 -> Map(0 -> "c", 1 -> "d"))"""),
		
		// map
		("map (flow)", "{a : b,c : d}", """Map("a" -> "b", "c" -> "d")"""),
		("map (flow trailing comma)", "{a : b, c : d,}", """Map("a" -> "b", "c" -> "d")"""),
		("map (flow with '?')", "{? a : b, ? }", """Map("a" -> "b", "" -> "")"""),
		("map (compact)", "a : b", """Map("a" -> "b")"""),
		("map (block)", "? a\n: b", """Map("a\n" -> "b\n")"""),
		("map (no '?')", "a : b\nc : d", """Map("a" -> "b", "c" -> "d")"""),
*/		
		("", "", """ "" """)
	)
	
	
	
	private val prefix:String = """
package com.rayrobdod.json.parser;

import java.text.ParseException;
import scala.collection.immutable.Map;
import org.scalatest.FunSpec;
import com.rayrobdod.json.builder.MapBuilder;

class YamlParserTest_Happy extends FunSpec {
	describe("YamlParser + MapBuilder can decode") {"""
		
	private val suffix:String = "\n\t}\n}"
	
	private val testStrings:Seq[String] = testValues.toSeq.map{abc =>
		val (name:String, source:String, expected:String) = abc
		
		"\n\t\tit (\"\"\"" + name + "\"\"\"" + """) {
			val source = """ + "\"\"\"" + source + "\"\"\"" + """
			val expected = """ + expected + """
			val result = new YamlParser(new MapBuilder()).parse(source)
			assertResult(expected){result}
		}
		it (""" + '"' + name + """ (reader)") {
			val source = new java.io.StringReader(""" + "\"\"\"" + source + "\"\"\"" + """)
			val expected = """ + expected + """
			val result = new YamlParser(new MapBuilder()).parse(source)
			assertResult(expected){result}
		}"""
	}
	
	
	val generateYamlParserTest = TaskKey[Seq[File]]("generateYamlParserTest")
	
	
	val settings = Seq(
		generateYamlParserTest in Test := {
			val file = (resourceManaged in Test).value / "YamlParserTest_Happy.scala"
			sbt.IO.write(file, prefix, UTF_8, false)
			testStrings.foreach{test => sbt.IO.write(file, test, UTF_8, true)}
			sbt.IO.write(file, suffix, UTF_8, true)
			Seq(file)
		},
		sourceGenerators in Test <+= generateYamlParserTest in Test
	)
	
}
