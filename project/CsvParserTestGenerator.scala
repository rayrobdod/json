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

object CsvParserTestGenerator {
	
	// stRec, stFie, norm, whit, quot, esc
	
	private val testValues:Seq[(String, String, String, String)] = Seq(
		("empty object", """ "" """, "Seq()", "CsvParser.csvCharacterMeanings"),
		("empty object with whitespace", """ "  " """, "Seq()", "CsvParser.csvCharacterMeanings"),
		("single element csv", """ "4" """, """Seq(Seq("4"))""", "CsvParser.csvCharacterMeanings"),
		("single element ascii", """ "4" """, """Seq(Seq("4"))""", "CsvParser.asciiCharacterMeanings"),
		("two elements csv", """ "4,5" """, """Seq(Seq("4", "5"))""", "CsvParser.csvCharacterMeanings"),
		("two elements ascii", """ "4\u001F6" """, """Seq(Seq("4", "6"))""", "CsvParser.asciiCharacterMeanings"),
		("two rows csv", """ "1,2\n3,4" """, """Seq(Seq("1", "2"),Seq("3", "4"))""", "CsvParser.csvCharacterMeanings"),
		("two rows ascii", """ "1\u001F2\u001E3\u001F4" """, """Seq(Seq("1", "2"),Seq("3", "4"))""", "CsvParser.asciiCharacterMeanings"),
		("ignorable handling", """ "  1  2  " """, """Seq(Seq("1  2"))""", "CsvParser.csvCharacterMeanings"),
		("quoted string", """ " \" 1 \" " """, """Seq(Seq(" 1 "))""", "CsvParser.csvCharacterMeanings"),
		("quoted string two elem", """ " \" 1 \", \",\" " """, """Seq(Seq(" 1 ", ","))""", "CsvParser.csvCharacterMeanings"),
		("escaped", """ " \\, " """, """Seq(Seq(","))""", "CsvParser.csvCharacterMeanings"),
		("escaped two elem", """ " \\, , \\, " """, """Seq(Seq(",",","))""", "CsvParser.csvCharacterMeanings"),
		("two rows csv w\\ whitespace", """ " 1 , 2 \n 3 , 4 " """, """Seq(Seq("1", "2"),Seq("3", "4"))""", "CsvParser.csvCharacterMeanings"),
		
		// LibreOffice, at least, ignores trailing commas and lines
		("two empty rows", """ "\n" """, """Seq(Seq(""))""", "CsvParser.csvCharacterMeanings"),
		("three empty fields", """ ",,," """, """Seq(Seq("","",""))""", "CsvParser.csvCharacterMeanings"),
		("2x2 empty", "\",,\\n,,\"", """Seq(Seq("",""),Seq("",""))""", "CsvParser.csvCharacterMeanings"),
		("2x2 empty w/ ending newline", "\",,\\n,,\\n\"", """Seq(Seq("",""),Seq("",""))""", "CsvParser.csvCharacterMeanings")
	)
	
	
	
	private val prefix:String = """
package com.rayrobdod.json.parser;

import java.text.ParseException;
import scala.collection.immutable.Map;
import org.scalatest.FunSpec;
import com.rayrobdod.json.builder.SeqBuilder;
import com.rayrobdod.json.builder.MapBuilder;

class CsvParserTest_Happy extends FunSpec {
	describe("CsvParser + MapBuilder can decode") {"""
		
	private val suffix:String = "\n\t}\n}"
	
	private val testStrings:Seq[String] = testValues.toSeq.map{abc =>
		val (name:String, source:String, expected:String, charMeans:String) = abc
		
		"\n\t\tit (\"\"\"" + name + "\"\"\"" + """) {
			val source = """ + source + """
			val expected = """ + expected + """
			val result = new CsvParser(new SeqBuilder(), """ + charMeans + """).parse(source)
			assertResult(expected){result}
		}"""
	}
	
	
	val generateCsvParserTest = TaskKey[Seq[File]]("generateCsvParserTest")
	
	
	val settings = Seq(
		generateCsvParserTest in Test := {
			val file = (resourceManaged in Test).value / "CsvParserTest_Happy.scala"
			sbt.IO.write(file, prefix, UTF_8, false)
			testStrings.foreach{test => sbt.IO.write(file, test, UTF_8, true)}
			sbt.IO.write(file, suffix, UTF_8, true)
			Seq(file)
		},
		sourceGenerators in Test <+= generateCsvParserTest in Test
	)
	
}
