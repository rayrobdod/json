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

object CborParserTestGenerator {
	
	private val testValues:Seq[(String, String, String)] = Seq(
		("false", """Array[Byte](0xF4.byteValue)""", "CborParser.ParseReturnValueSimple(JsonValueBoolean(false))"),
		("true", """Array[Byte](0xF5.byteValue)""", "CborParser.ParseReturnValueSimple(JsonValueBoolean(true))"),
		("null", """Array[Byte](0xF6.byteValue)""", "CborParser.ParseReturnValueSimple(JsonValueNull)"),
		("unknown", """Array[Byte](0xE4.byteValue)""", "CborParser.ParseReturnValueUnknownSimple(4)"),
		("unknown (+byte)", """ hexArray"F842" """, "CborParser.ParseReturnValueUnknownSimple(0x42)"),
		("endOfObject", """Array[Byte](0xFF.byteValue)""", "CborParser.ParseReturnValueEndOfIndeterminateObject()"),
		("integer 0", """Array[Byte](0)""", "CborParser.ParseReturnValueSimple(JsonValueNumber(0))"),
		("integer 1", """Array[Byte](1)""", "CborParser.ParseReturnValueSimple(JsonValueNumber(1))"),
		("integer 15", """Array[Byte](15)""", "CborParser.ParseReturnValueSimple(JsonValueNumber(15))"),
		("integer 23", """Array[Byte](23)""", "CborParser.ParseReturnValueSimple(JsonValueNumber(23))"),
		("integer 0x12", """Array[Byte](24, 0x12)""", "CborParser.ParseReturnValueSimple(JsonValueNumber(0x12))"),
		("integer 0x1234", """Array[Byte](25, 0x12, 0x34)""", "CborParser.ParseReturnValueSimple(JsonValueNumber(0x1234))"),
		("integer 0x12345678", """Array[Byte](26, 0x12, 0x34, 0x56, 0x78)""", "CborParser.ParseReturnValueSimple(JsonValueNumber(0x12345678))"),
		("integer 0x1234567890ABCDEF", """Array(27, 0x12, 0x34, 0x56, 0x78, 0x90, 0xAB, 0xCD, 0xEF).map{_.byteValue}""", "CborParser.ParseReturnValueSimple(JsonValueNumber(0x1234567890ABCDEFl))"),
		("integer 43", """Array[Byte](24, 43)""", "CborParser.ParseReturnValueSimple(JsonValueNumber(43))"),
		("integer -1", """Array[Byte](0x20)""", "CborParser.ParseReturnValueSimple(JsonValueNumber(-1))"),
		("integer -5", """Array[Byte](0x24)""", "CborParser.ParseReturnValueSimple(JsonValueNumber(-5))"),
	//	("halffloat 1.5", """ hexArray"F93C00" """, "CborParser.ParseReturnValueSimple(JsonValueNumber(1.5))"),
		("float 1.5", """ hexArray"FA3FC00000" """, "CborParser.ParseReturnValueSimple(JsonValueNumber(1.5))"),
		("doublefloat -4.1", """ hexArray"fbc010666666666666" """, "CborParser.ParseReturnValueSimple(JsonValueNumber(-4.1))"),
		("byte string 0", """Array[Byte](0x40)""", "CborParser.ParseReturnValueSimple(JsonValueByteStr(Array[Byte]()))"),
		("byte string 4", """Array[Byte](0x44, 1,2,3,4)""", "CborParser.ParseReturnValueSimple(JsonValueByteStr(Array[Byte](1,2,3,4)))"),
		("byte string 30", """Array[Byte](0x58, 30) ++ (1 to 30).map{_.byteValue}""", "CborParser.ParseReturnValueSimple(JsonValueByteStr((1.byteValue to 30.byteValue).map{_.byteValue}.toArray))"),
		("byte string INDET", """ hexArray"5F44AABBCCDD43EEFF99FF" """, """CborParser.ParseReturnValueSimple(JsonValueByteStr(hexArray"AABBCCDDEEFF99")) """),
		("char string 0", """Array[Byte](0x60)""", "CborParser.ParseReturnValueSimple(JsonValueString(\"\")) "),
		("char string 5", """Array(0x65, 'h', 'e', 'l', 'l', 'o').map{_.byteValue}""", """CborParser.ParseReturnValueSimple(JsonValueString("hello")) """),
		("char string multibyte char", """hexArray"63e6b0b4" """, """CborParser.ParseReturnValueSimple(JsonValueString("\u6c34") )"""),
		("char string INDET", """Array(0x7F, 0x62, 'h', 'e', 0x63, 'l', 'l', 'o', 0xFF).map{_.byteValue}""", """CborParser.ParseReturnValueSimple(JsonValueString("hello") )"""),
		("array 0", """ hexArray"80" """, "CborParser.ParseReturnValueComplex(Map())"),
		("array 1", """ hexArray"8121" """, """CborParser.ParseReturnValueComplex(Map(JsonValue(0) -> JsonValue(-2)))"""),
		("array 4", """ hexArray"8400010203" """, """CborParser.ParseReturnValueComplex(Map(JsonValue(0) -> JsonValue(0), JsonValue(1) -> JsonValue(1), JsonValue(2) -> JsonValue(2), JsonValue(3) -> JsonValue(3)))"""),
		("array INDET", """ hexArray"9F00010203FF" """, """CborParser.ParseReturnValueComplex(Map(JsonValue(0) -> JsonValue(0), JsonValue(1) -> JsonValue(1), JsonValue(2) -> JsonValue(2), JsonValue(3) -> JsonValue(3)))"""),
		("object 0", """ hexArray"A0" """, "CborParser.ParseReturnValueComplex(Map())"),
		("object 1", """ hexArray"A10405" """, """CborParser.ParseReturnValueComplex(Map(JsonValue(4) -> JsonValue(5)))"""),
		("object 2", """ hexArray"A2600061651865" """, """CborParser.ParseReturnValueComplex(Map(JsonValue("") -> JsonValue(0), JsonValue("e") -> JsonValue(0x65)))"""),
		("object INDET", """ hexArray"BF600061651865FF" """, """CborParser.ParseReturnValueComplex(Map(JsonValue("") -> JsonValue(0), JsonValue("e") -> JsonValue(0x65)))"""),
		("tag self-describing", """ hexArray"d9d9f780" """, "CborParser.ParseReturnValueTaggedValue(55799, CborParser.ParseReturnValueComplex(Map()))")
		
	)
	
	
	
	private val prefix:String = """
package com.rayrobdod.json.parser;

import java.text.ParseException;
import scala.collection.immutable.Map;
import org.scalatest.FunSpec;
import com.rayrobdod.json.builder.MapBuilder;
import com.rayrobdod.json.union.JsonValue
import com.rayrobdod.json.union.JsonValue._

class CborParserTest_Happy extends FunSpec {

	describe("CborParser + MapBuilder can decode") {"""
		
	private val suffix:String = "\n\t}\n}"
	
	private val testStrings:Seq[String] = testValues.toSeq.map{abc =>
		val (name:String, source:String, expected:String) = abc
		
		"\n\t\tit (\"\"\"" + name + "\"\"\"" + """) {
			val source = new java.io.DataInputStream(
				new java.io.ByteArrayInputStream(
					""" + source + """
				)
			)
			val expected = """ + expected + """
			val result = new CborParser().parseDetailed(new MapBuilder(), source)
			assertResult(expected){result}
		}"""
	}
	
	
	val generateCborParserTest = TaskKey[Seq[File]]("generateCborParserTest")
	
	
	val settings = Seq(
		generateCborParserTest in Test := {
			val file = (resourceManaged in Test).value / "CborParserTest_Happy.scala"
			sbt.IO.write(file, prefix, UTF_8, false)
			testStrings.foreach{test => sbt.IO.write(file, test, UTF_8, true)}
			sbt.IO.write(file, suffix, UTF_8, true)
			Seq(file)
		},
		sourceGenerators in Test <+= generateCborParserTest in Test
	)
	
}
