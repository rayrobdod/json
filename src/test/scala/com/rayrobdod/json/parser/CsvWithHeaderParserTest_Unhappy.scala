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
package com.rayrobdod.json.parser

import org.scalatest.FunSpec
import com.rayrobdod.json.union.{StringOrInt, ParserRetVal}
import com.rayrobdod.json.union.Failures.EnforcedFailure
import com.rayrobdod.json.builder._

class CsvWithHeaderParserTest_Unhappy extends FunSpec {
	describe("CsvWithHeaderParser") {
		it ("""Throw builder immediate""") {
			val source = "g,h,i\na,b,c\nd,e,f\n"
			assertResult(ParserRetVal.BuilderFailure(EnforcedFailure)){  // idx == 6
				new CsvWithHeaderParser().parse(new ThrowBuilder[StringOrInt, String], source)
			}
		}
		it ("""Throw builder indirect""") {
			val source = "g,h,i\na,b,c\nd,e,f\n"
			assertResult(ParserRetVal.BuilderFailure(EnforcedFailure)){  // idx == 12
				new CsvWithHeaderParser().parse(MapBuilder.apply[StringOrInt, String, EnforcedFailure.type, Any]({x:StringOrInt => x match {
					case StringOrInt.Right(1) => new MapBuilder.MapChildBuilder[StringOrInt, String, EnforcedFailure.type, Any, Any](new ThrowBuilder[StringOrInt, String].mapValue[String], {x:Any => x})
					case _ => new MapBuilder.MapChildBuilder[StringOrInt, String, EnforcedFailure.type, MapBuilder.RecursiveSubjectType[StringOrInt, String], Any](MapBuilder[StringOrInt, String], {x:Any => x})
				}}), source)
			}
		}
	}
	
}
