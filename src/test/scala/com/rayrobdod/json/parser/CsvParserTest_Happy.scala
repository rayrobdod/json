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
package com.rayrobdod.json.parser;

import java.text.ParseException;
import scala.collection.immutable.Map;
import org.scalatest.FunSpec;
import com.rayrobdod.json.builder.PrimitiveSeqBuilder;
import com.rayrobdod.json.builder.SeqBuilder;
import com.rayrobdod.json.builder.MapBuilder;
import java.nio.charset.StandardCharsets.UTF_8

final class CsvParserTest_Happy extends FunSpec {
	
	// stRec, stFie, norm, whit, quot, esc
	
	private val testValues:Seq[(String, String, Seq[Seq[String]], CsvParser.CharacterMeanings)] = Seq(
		("empty object", "", Seq(), CsvParser.csvCharacterMeanings),
		("empty object with whitespace csv", "  ", Seq(Seq()), CsvParser.csvCharacterMeanings),
		("empty object with whitespace tsv", "  ", Seq(Seq()), CsvParser.tsvCharacterMeanings),
		("empty object with byteordermark csv", "\ufeff", Seq(Seq()), CsvParser.csvCharacterMeanings),
		("empty object with byteordermark tsv", "\ufeff", Seq(Seq()), CsvParser.tsvCharacterMeanings),
		("single element csv", "4", Seq(Seq("4")), CsvParser.csvCharacterMeanings),
		("single element tsv", "4", Seq(Seq("4")), CsvParser.tsvCharacterMeanings),
		("single element ascii", "4", Seq(Seq("4")), CsvParser.asciiCharacterMeanings),
		("two elements csv", "4,5", Seq(Seq("4", "5")), CsvParser.csvCharacterMeanings),
		("two elements tsv", "4\t5", Seq(Seq("4", "5")), CsvParser.tsvCharacterMeanings),
		("two elements ascii", "4\u001F6", Seq(Seq("4", "6")), CsvParser.asciiCharacterMeanings),
		("two rows csv", "1,2\n3,4", Seq(Seq("1", "2"),Seq("3", "4")), CsvParser.csvCharacterMeanings),
		("two rows tsv", "1\t2\n3\t4", Seq(Seq("1", "2"),Seq("3", "4")), CsvParser.tsvCharacterMeanings),
		("two rows ascii", "1\u001F2\u001E3\u001F4", Seq(Seq("1", "2"),Seq("3", "4")), CsvParser.asciiCharacterMeanings),
		("ignorable handling", "  1  2  ", Seq(Seq("1  2")), CsvParser.csvCharacterMeanings),
		("quoted string", " \" 1 \" ", Seq(Seq(" 1 ")), CsvParser.csvCharacterMeanings),
		("quoted string two elem", " \" 1 \", \",\" ", Seq(Seq(" 1 ", ",")), CsvParser.csvCharacterMeanings),
		("escaped", " \\, ", Seq(Seq(",")), CsvParser.csvCharacterMeanings),
		("escaped two elem", " \\, , \\, ", Seq(Seq(",",",")), CsvParser.csvCharacterMeanings),
		("two rows csv w\\ whitespace", " 1 , 2 \n 3 , 4 ", Seq(Seq("1", "2"),Seq("3", "4")), CsvParser.csvCharacterMeanings),
		("does not error if input ends while in a quote", "1 , 2 \n 3 , \"4 ", Seq(Seq("1", "2"),Seq("3", "4 ")), CsvParser.csvCharacterMeanings),
		("does not error if input ends in middle of escape", "1 , 2 \n 3 , 4\\", Seq(Seq("1", "2"),Seq("3", "4" )), CsvParser.csvCharacterMeanings),
		
		// LibreOffice, at least, ignores trailing commas and lines
		("two empty rows", "\n", Seq(Seq()), CsvParser.csvCharacterMeanings),
		("three empty fields", ",,,", Seq(Seq("","","")), CsvParser.csvCharacterMeanings),
		("2x2 empty", ",,\n,,", Seq(Seq("",""),Seq("","")), CsvParser.csvCharacterMeanings),
		("2x2 empty w/ ending newline", ",,\n,,\n", Seq(Seq("",""),Seq("","")), CsvParser.csvCharacterMeanings)
	)
	
	
	
	describe("CsvParser + MapBuilder can decode") {
		testValues.foreach{abc =>
			val (name:String, source2:String, expected:Seq[Seq[String]], charMeans:CsvParser.CharacterMeanings) = abc
			
			it (name) {
				val source = source2
				val result = new CsvParser(charMeans).parse(new SeqBuilder(new PrimitiveSeqBuilder[String]), source).fold({x => x},{x => x},{(a,b) => a})
				assertResult(expected){result}
			}
			it (name + " (reader)") {
				val source = new java.io.StringReader(source2)
				val result = new CsvParser(charMeans).parse(new SeqBuilder(new PrimitiveSeqBuilder[String]), source).fold({x => x},{x => x},{(a,b) => a})
				assertResult(expected){result}
			}
		}
	}
	
}
