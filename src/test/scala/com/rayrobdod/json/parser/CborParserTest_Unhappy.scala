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
import java.text.ParseException;
import scala.collection.immutable.{Seq, Map};
import com.rayrobdod.json.union.{CborValue, ParserRetVal}
import com.rayrobdod.json.builder._

class CborParserTest_Unhappy extends FunSpec {
	describe("CborParser") {
		it ("""errors when told to decode a half float""") {
			val source = hexArray"F93C00"
			assertFailure("", 0){
				new CborParser().parse(MapBuilder.apply, byteArray2DataInput(source))
			}
		}
		it ("""errors when array is incomplete""") {
			val source = Array[Byte](0x58, 30) ++ (1 to 10).map{_.byteValue}
			assertFailure("", 0){
				new CborParser().parse(MapBuilder.apply, byteArray2DataInput(source))
			}
		}
		it ("""illegal additional info field""") {
			val source = Array[Byte](28) ++ (1 to 50).map{_.byteValue}
			assertFailure("", 0){
				new CborParser().parse(MapBuilder.apply, byteArray2DataInput(source))
			}
		}
		it ("""errors when INDET byte string contains non-string values""") {
			val source = hexArray"5F44AABBCCDD21FF"
			assertFailure("", 0){
				new CborParser().parse(MapBuilder.apply, byteArray2DataInput(source))
			}
		}
		it ("errors when INDET utf-8 string contains non-string values") {
			val source = hexArray"7F00FF"
			val builder = new ThrowBuilder[CborValue, CborValue]
			assertFailure("", 0){
				new CborParser().parse(new PrimitiveSeqBuilder, byteArray2DataInput(source))
			}
		}
		it ("errors when an integer has an indeterminate length") {
			val source = hexArray"1F"
			assertFailure("", 0){
				new CborParser().parse(MapBuilder.apply, byteArray2DataInput(source))
			}
		}
		it ("errors when an integer has an 1E-type length") {
			val source = hexArray"1E"
			assertFailure("", 0){
				new CborParser().parse(MapBuilder.apply, byteArray2DataInput(source))
			}
		}
		it ("errors when an integer has an 1D-type length") {
			val source = hexArray"1E"
			assertFailure("", 0){
				new CborParser().parse(MapBuilder.apply, byteArray2DataInput(source))
			}
		}
		it ("errors when an integer has an 1C-type length") {
			val source = hexArray"1E"
			assertFailure("", 0){
				new CborParser().parse(MapBuilder.apply, byteArray2DataInput(source))
			}
		}
		it ("errors when a byte array has an 1C-type length") {
			val source = hexArray"5C"
			assertFailure("", 0){
				new CborParser().parse(MapBuilder.apply, byteArray2DataInput(source))
			}
		}
		it ("errors when a string array has an 1E-type length") {
			val source = hexArray"7E"
			assertFailure("", 0){
				new CborParser().parse(MapBuilder.apply, byteArray2DataInput(source))
			}
		}
		it ("errors when a simple value has an 1C-type length") {
			val source = hexArray"FC"
			assertFailure("", 0){
				new CborParser().parse(MapBuilder.apply, byteArray2DataInput(source))
			}
		}
		it ("errors when a negative integer has an indeterminate length") {
			val source = hexArray"3F"
			assertFailure("", 0){
				new CborParser().parse(MapBuilder.apply, byteArray2DataInput(source))
			}
		}
		it ("errors when a tag has an indeterminate length") {
			val source = hexArray"DF"
			assertFailure("", 0){
				new CborParser().parse(MapBuilder.apply, byteArray2DataInput(source))
			}
		}
		it ("errors upon finding a standalone END_OF_INDETERMINATE_OBJECT") {
			val source = hexArray"FF"
			assertFailure("", 0){
				new CborParser().parse(MapBuilder.apply, byteArray2DataInput(source))
			}
		}
		
		it ("can handle an indeterminate array with complex values") {
			val expected = ParserRetVal.Complex(Seq(Seq(CborValue(0))))
			val source = hexArray"9F8100FF"
			val builder = new SeqBuilder[CborValue, CborValue, Seq[CborValue]](new PrimitiveSeqBuilder[CborValue, CborValue])
			assertResult(expected){
				new CborParser().parse(builder, byteArray2DataInput(source))
			}
		}
		it ("can handle an indeterminate object with complex values") {
			val expected = ParserRetVal.Complex(Map(CborValue(10) -> Left(Seq(CborValue(0)))))
			val source = hexArray"BF 0A 8100 FF"
			val builder = MapBuilder(new PrimitiveSeqBuilder[CborValue, CborValue])
			assertResult(expected){
				new CborParser().parse(builder, byteArray2DataInput(source))
			}
		}
		it ("cannot handle an indeterminate object with complex keys") {
			val expected = Map(Seq(CborValue(0)) -> CborValue(10))
			val source = hexArray"BF 8100 0A FF"
			val builder = MapBuilder(new PrimitiveSeqBuilder[CborValue, CborValue])
			assertFailure("", 0){
				new CborParser().parse(builder, byteArray2DataInput(source))
			}
		}
		
		it ("IDENT Array of tag fails") {
			val source = hexArray"9F d9d9f7 00 FF"
			assertFailure("", 0){
				new CborParser().parse(new PrimitiveSeqBuilder, byteArray2DataInput(source))
			}
		}
		it ("IDENT Object with non-primitive key fails") {
			val source = hexArray"A1 80 00"
			assertFailure("", 0){
				new CborParser().parse(new PrimitiveSeqBuilder, byteArray2DataInput(source))
			}
		}
		it ("Object with non-primitive key fails") {
			val source = hexArray"BF 80 00 FF"
			assertFailure("", 0){
				new CborParser().parse(new PrimitiveSeqBuilder, byteArray2DataInput(source))
			}
		}
	}
	
	def assertFailure(msg:String, idx:Int)(result:ParserRetVal[_,_]):Unit = result match {
		case ParserRetVal.Failure(msg2, idx2) => {
	//		assertResult(msg){msg2}
			assertResult(idx){idx2}
		}
		case x => fail("Not a Failure: " + x)
	}
}
