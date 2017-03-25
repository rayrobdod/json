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

import org.scalatest.FunSpec;
import scala.collection.immutable.Seq
import com.rayrobdod.json.union.{CborValue, ParserRetVal}
import com.rayrobdod.json.builder._
import com.rayrobdod.json.testing.HexArrayStringConverter
import com.rayrobdod.json.parser.CborParser.Failures._
import com.rayrobdod.json.union.Failures.EnforcedFailure

class CborParserTest_Unhappy extends FunSpec {
	
	private val parser = new CborParser()
	private val mapBuilder = MapBuilder[CborValue, CborValue].mapResult{x => x:Any}
	private val seq1Builder = new PrimitiveSeqBuilder[CborValue].mapResult{x => x:Any}
	private val seq2Builder = new SeqBuilder(new PrimitiveSeqBuilder[CborValue]).mapResult{x => x:Any}
	
	private val failureCases:Seq[(String, Array[Byte], Builder[CborValue, CborValue, Any, Any], CborParser.Failures)] = Seq(
		  ("errors when array is incomplete", Array[Byte](0x58, 30) ++ (1 to 10).map{_.byteValue}, seq1Builder, IOException(null))
		, ("illegal additional info field", Array[Byte](28) ++ (1 to 50).map{_.byteValue}, seq1Builder, IllegalAdditionalInfoField(28))
		, ("errors when INDET byte string contains non-string values", hexArray"5F44AABBCCDD21FF", mapBuilder, IndeterminateStringSubstringsNotStrings)
		, ("errors when INDET utf-8 string contains non-string values", hexArray"7F00FF", seq1Builder, IndeterminateStringSubstringsNotStrings)
		, ("errors when an integer has an indeterminate length", hexArray"1F", mapBuilder, IndeterminateIntegerValue)
		, ("errors when an integer has an 1E-type length", hexArray"1E", mapBuilder, IllegalAdditionalInfoField(30))
		, ("errors when an integer has an 1D-type length", hexArray"1D", mapBuilder, IllegalAdditionalInfoField(29))
		, ("errors when an integer has an 1C-type length", hexArray"1C", mapBuilder, IllegalAdditionalInfoField(28))
		, ("errors when a byte array has an 1C-type length", hexArray"5C", mapBuilder, IllegalAdditionalInfoField(28))
		, ("errors when a string array has an 1E-type length", hexArray"7E", mapBuilder, IllegalAdditionalInfoField(30))
		, ("errors when a simple value has an 1C-type length", hexArray"FC", mapBuilder, IllegalAdditionalInfoField(28))
		, ("errors when a negative integer has an indeterminate length", hexArray"3F", mapBuilder, IndeterminateIntegerValue)
		, ("errors when a tag has an indeterminate length", hexArray"DF", mapBuilder, IndeterminateTagValue)
		, ("errors upon finding a standalone END_OF_INDETERMINATE_OBJECT", hexArray"FF", mapBuilder, NonPublicValue)
		
		, ("IDENT Object with non-primitive key fails", hexArray"A1 80 00", seq1Builder, NonSimpleMapKey)
		, ("Object with non-primitive key fails", hexArray"BF 80 00 FF", seq1Builder, NonSimpleMapKey)
		, ("cannot handle an indeterminate object with complex keys", hexArray"BF 8100 0A FF", mapBuilder, NonSimpleMapKey)
		
		, ("IDENT Array of unknown tag fails", hexArray"9F d9d9f6 00 FF", mapBuilder, NonPublicValue) // or d9d9f7 with CborParser.TagMatcher.empty
	)
	
	
	describe("CborParser") {
		failureCases.foreach{abcde =>
			val (name, source, builder, expectedFailures) = abcde
			
			it (name) {
				assertResult(ParserRetVal.ParserFailure(expectedFailures)){
					parser.parse(builder, byteArray2DataInput(source))
				}
			}
		}
	}
}
