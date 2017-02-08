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

import scala.collection.immutable.{Seq, Map => IMap}
import org.scalatest.FunSpec
import com.rayrobdod.json.builder.SeqBuilder
import com.rayrobdod.json.builder.MapBuilder
import com.rayrobdod.json.union.StringOrInt
import com.rayrobdod.json.union.ParserRetVal.Complex

final class CsvWithHeaderParserTest_Happy extends FunSpec {
	private[this] def Map(es:(String, String)*):IMap[StringOrInt, Either[Nothing, String]] = {
		es.map{case (a,b) => StringOrInt(a) -> Right(b)}.toMap
	}
	
	private val testValues:Seq[(String, String, Seq[IMap[StringOrInt, Either[Nothing, String]]], CsvParser.CharacterMeanings)] = Seq(
		("empty object", "", Seq(), CsvParser.csvCharacterMeanings),
		("empty object with whitespace csv", "  ", Seq(), CsvParser.csvCharacterMeanings),
		("empty object with whitespace tsv", "  ", Seq(), CsvParser.tsvCharacterMeanings),
		("empty object with byteordermark csv", "\ufeff", Seq(), CsvParser.csvCharacterMeanings),
		("empty object with byteordermark tsv", "\ufeff", Seq(), CsvParser.tsvCharacterMeanings),
		("header-only single-element", "A", Seq(), CsvParser.csvCharacterMeanings),
		("header-only multiple-element (comma)", "A,B,C", Seq(), CsvParser.csvCharacterMeanings),
		("header-only multiple-element (tab)", "A\tB\tC", Seq(), CsvParser.tsvCharacterMeanings),
		("1x1 (comma)", "A\n1", Seq(Map("A" -> "1")), CsvParser.csvCharacterMeanings),
		("1x1 (tab)", "A\n1", Seq(Map("A" -> "1")), CsvParser.tsvCharacterMeanings),
		("3x3 (comma)", "A,B,C\n1,2,3\n4,5,6\n7,8,9", Seq(Map("A" -> "1", "B" -> "2", "C" -> "3"), Map("A" -> "4", "B" -> "5", "C" -> "6"), Map("A" -> "7", "B" -> "8", "C" -> "9")), CsvParser.csvCharacterMeanings),
		("3x3 (tab)", "A\tB\tC\n1\t2\t3\n4\t5\t6\n7\t8\t9", Seq(Map("A" -> "1", "B" -> "2", "C" -> "3"), Map("A" -> "4", "B" -> "5", "C" -> "6"), Map("A" -> "7", "B" -> "8", "C" -> "9")), CsvParser.tsvCharacterMeanings),
		("ignorable handling", " A \n  1  2  ", Seq(Map("A" -> "1  2")), CsvParser.csvCharacterMeanings),
		("quoted string", " \" A \" \n \" 1 \" ", Seq(Map(" A " -> " 1 ")), CsvParser.csvCharacterMeanings),
		("escaped", " A\n \\, ", Seq(Map("A" -> ",")), CsvParser.csvCharacterMeanings),
		("two rows csv w\\ whitespace", " A , B \n 1 , 2 \n 3 , 4 ", Seq(Map("A"->"1","B"-> "2"), Map("A"->"3", "B"->"4")), CsvParser.csvCharacterMeanings),
		("does not error if input ends while in a quote", "1 , 2 \n 3 , \"4 ", Seq(Map("1"->"3", "2"->"4 ")), CsvParser.csvCharacterMeanings),
		("does not error if input ends in middle of escape", "1 , 2 \n 3 , 4\\", Seq(Map("1"->"3", "2"->"4")), CsvParser.csvCharacterMeanings),
		
		// LibreOffice, at least, ignores trailing commas and lines
		("two empty rows", "\n\n", Seq(Map()), CsvParser.csvCharacterMeanings),
		("three empty fields", "A,B,C\n,,,", Seq(Map("A"->"","B"->"","C"->"")), CsvParser.csvCharacterMeanings),
		("2x2 empty", "A,B,\n,,\n,,", Seq(Map("A"->"","B"->""),Map("A"->"","B"->"")), CsvParser.csvCharacterMeanings),
		("2x2 empty w/ ending newline", "A,B,\n,,\n,,\n", Seq(Map("A"->"","B"->""),Map("A"->"","B"->"")), CsvParser.csvCharacterMeanings)
	)
	
	
	
	describe("CsvParser + MapBuilder can decode") {
		testValues.foreach{abc =>
			val (name:String, source2:String, expected:Seq[IMap[StringOrInt, Either[Nothing, String]]], charMeans:CsvParser.CharacterMeanings) = abc
			
			it (name) {
				val source = source2
				val result = new CsvWithHeaderParser(charMeans).parse(new SeqBuilder(MapBuilder.apply[StringOrInt, String]), source)
				assertResult(Complex(expected)){result}
			}
			it (name + " (reader)") {
				val source = new java.io.StringReader(source2)
				val result = new CsvWithHeaderParser(charMeans).parse(new SeqBuilder(MapBuilder.apply[StringOrInt, String]), source)
				assertResult(Complex(expected)){result}
			}
		}
	}
}
