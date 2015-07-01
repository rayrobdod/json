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
		("false", """Array[Byte](0xF4.byteValue)""", "false"),
		("true", """Array[Byte](0xF5.byteValue)""", "true"),
		("null", """Array[Byte](0xF6.byteValue)""", "null"),
		("unknown", """Array[Byte](0xE4.byteValue)""", "CborParser.UnknownSimpleValue(4)"),
		("endOfObject", """Array[Byte](0xFF.byteValue)""", "CborParser.EndOfIndeterminateObject()"),
		("integer 0", """Array[Byte](0)""", "0"),
		("integer 1", """Array[Byte](1)""", "1"),
		("integer 15", """Array[Byte](15)""", "15"),
		("integer 23", """Array[Byte](23)""", "23"),
		("integer 0x12", """Array[Byte](24, 0x12)""", "0x12"),
		("integer 0x1234", """Array[Byte](25, 0x12, 0x34)""", "0x1234"),
		("integer 0x12345678", """Array[Byte](26, 0x12, 0x34, 0x56, 0x78)""", "0x12345678"),
		("integer 0x1234567890ABCDEF", """Array(27, 0x12, 0x34, 0x56, 0x78, 0x90, 0xAB, 0xCD, 0xEF).map{_.byteValue}""", "0x1234567890ABCDEFl"),
		("integer 43", """Array[Byte](24, 43)""", "43"),
		("integer -1", """Array[Byte](0x20)""", "-1"),
		("integer -5", """Array[Byte](0x24)""", "-5"),
	//	("halffloat 1.5", """ hexArray"F93C00" """, "1.5"),
		("float 1.5", """ hexArray"FA3FC00000" """, "1.5"),
		("doublefloat -4.1", """ hexArray"fbc010666666666666" """, "-4.1"),
		("byte string 0", """Array[Byte](0x40)""", "Array[Byte]()"),
		("byte string 4", """Array[Byte](0x44, 1,2,3,4)""", "Array[Byte](1,2,3,4)"),
		("byte string 30", """Array[Byte](0x58, 30) ++ (1 to 30).map{_.byteValue}""", "(1.byteValue to 30.byteValue)"),
		("byte string INDET", """ hexArray"5F44AABBCCDD43EEFF99FF" """, """ hexArray"AABBCCDDEEFF99" """),
		("char string 0", """Array[Byte](0x60)""", " \"\" "),
		("char string 5", """Array(0x65, 'h', 'e', 'l', 'l', 'o').map{_.byteValue}""", """ "hello" """),
		("char string multibyte char", """hexArray"63e6b0b4" """, """ "\u6c34" """),
		("char string INDET", """Array(0x7F, 0x62, 'h', 'e', 0x63, 'l', 'l', 'o', 0xFF).map{_.byteValue}""", """ "hello" """),
		("array 0", """ hexArray"80" """, "Map()"),
		("array 1", """ hexArray"8121" """, """Map("0" -> -2)"""),
		("array 4", """ hexArray"8400010203" """, """Map("0" -> 0, "1" -> 1, "2" -> 2, "3" -> 3)"""),
		("array INDET", """ hexArray"9F00010203FF" """, """Map("0" -> 0, "1" -> 1, "2" -> 2, "3" -> 3)"""),
		("object 0", """ hexArray"A0" """, "Map()"),
		("object 1", """ hexArray"A10405" """, """Map("4" -> 5)"""),
		("object 2", """ hexArray"A2600061651865" """, """Map("" -> 0, "e" -> 0x65)"""),
		("object INDET", """ hexArray"BF600061651865FF" """, """Map("" -> 0, "e" -> 0x65)"""),
		("tag self-describing", """ hexArray"d9d9f780" """, "CborParser.TaggedValue(55799, Map())")
		
	)
	
	
	
	private val prefix:String = """
package com.rayrobdod.json.parser;

import java.text.ParseException;
import scala.collection.immutable.Map;
import org.scalatest.FunSpec;
import com.rayrobdod.json.builder.MapBuilder;

object CborParserTest_Happy {
	// String Interpolation
	implicit class HexArrayStringConverter(val sc: StringContext) extends AnyVal {
		def hexArray(args: Any*):Array[Byte] = {
			((sc.parts.head):String).filter{x => ('A' <= x && x <= 'F') || ('a' <= x && x <= 'f') || ('0' <= x && x <= '9')}.grouped(2).map{x => Integer.parseInt(x, 16)}.map{_.byteValue}.toArray
		}
	}
}

class CborParserTest_Happy extends FunSpec {
	import CborParserTest_Happy.HexArrayStringConverter

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
			val result = new CborParser(new MapBuilder()).parse(source)
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
