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
package com.rayrobdod.json.parser;

import org.scalatest.FunSpec;
import java.text.ParseException;
import scala.collection.immutable.Map;
import com.rayrobdod.json.union.JsonValue;
import com.rayrobdod.json.builder.MapBuilder;

class CborParserTest_Unhappy extends FunSpec {
	describe("CborParser") {
		it ("""errors when told to decode a half float""") {
			val source = hexArray"F93C00"
			val ex = intercept[UnsupportedOperationException]{
				new CborParser().parse(new MapBuilder(), byteArray2DataInput(source))
			}
		}
		it ("""errors when array is incomplete""") {
			val source = Array[Byte](0x58, 30) ++ (1 to 10).map{_.byteValue}
			val ex = intercept[java.io.EOFException]{
				new CborParser().parse(new MapBuilder(), byteArray2DataInput(source))
			}
		}
		it ("""illegal additional info field""") {
			val source = Array[Byte](28) ++ (1 to 50).map{_.byteValue}
			val ex = intercept[ParseException]{
				new CborParser().parse(new MapBuilder(), byteArray2DataInput(source))
			}
		}
		it ("""errors when INDET byte string contains non-string values""") {
			val source = hexArray"5F44AABBCCDD21FF"
			val ex = intercept[ClassCastException]{
				new CborParser().parse(new MapBuilder(), byteArray2DataInput(source))
			}
		}
		it ("errors when an integer has an indeterminate length") {
			val source = hexArray"1F"
			val ex = intercept[UnsupportedOperationException]{
				new CborParser().parse(new MapBuilder(), byteArray2DataInput(source))
			}
		}
		it ("errors when an integer has an 1E-type length") {
			val source = hexArray"1E"
			val ex = intercept[ParseException]{
				new CborParser().parse(new MapBuilder(), byteArray2DataInput(source))
			}
		}
		it ("errors when an integer has an 1D-type length") {
			val source = hexArray"1E"
			val ex = intercept[ParseException]{
				new CborParser().parse(new MapBuilder(), byteArray2DataInput(source))
			}
		}
		it ("errors when an integer has an 1C-type length") {
			val source = hexArray"1E"
			val ex = intercept[ParseException]{
				new CborParser().parse(new MapBuilder(), byteArray2DataInput(source))
			}
		}
		it ("errors when a byte array has an 1C-type length") {
			val source = hexArray"5C"
			val ex = intercept[ParseException]{
				new CborParser().parse(new MapBuilder(), byteArray2DataInput(source))
			}
		}
		it ("errors when a string array has an 1E-type length") {
			val source = hexArray"7E"
			val ex = intercept[ParseException]{
				new CborParser().parse(new MapBuilder(), byteArray2DataInput(source))
			}
		}
		it ("errors when a simple value has an 1C-type length") {
			val source = hexArray"FC"
			val ex = intercept[ParseException]{
				new CborParser().parse(new MapBuilder(), byteArray2DataInput(source))
			}
		}
		it ("errors when a negative integer has an indeterminate length") {
			val source = hexArray"3F"
			val ex = intercept[UnsupportedOperationException]{
				new CborParser().parse(new MapBuilder(), byteArray2DataInput(source))
			}
		}
		it ("errors when a tag has an indeterminate length") {
			val source = hexArray"DF"
			val ex = intercept[UnsupportedOperationException]{
				new CborParser().parse(new MapBuilder(), byteArray2DataInput(source))
			}
		}
	}
}
