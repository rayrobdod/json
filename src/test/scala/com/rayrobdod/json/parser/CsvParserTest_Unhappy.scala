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
import scala.collection.immutable.Map
import com.rayrobdod.json.union.ParserRetVal
import com.rayrobdod.json.builder._

class CsvParserTest_Unhappy extends FunSpec {
	describe("CsvParser") {
		it ("""Throw builder immediate""") {
			val source = "a,b,c\nd,e,f\n"
			assertFailureParse("",0){
				new CsvParser().parse(new ThrowBuilder[Int, String], source)
			}
		}
		it ("""Throw builder indirect""") {
			val source = "a,b,c\nd,e,f\n"
			assertFailureParse("",6){
				new CsvParser().parse(MapBuilder.apply2[Int, String, Any]({x:Int => x match {
					case 1 => new MapBuilder.MapChildBuilder[Int, String, Any, Any](new ThrowBuilder[Int, String].mapValue[String], {x:Any => x})
					case _ => new MapBuilder.MapChildBuilder[Int, String, MapBuilder.RecursiveSubjectType[Int,String], Any](MapBuilder[Int, String], {x:Any => x})
				}}), source)
			}
		}
	}
	
	
	def assertFailureParse(msg:String, idx:Int)(result:ParserRetVal[_,_]):Unit = result match {
		case ParserRetVal.Failure(msg2, idx2) => {
	//		assertResult(msg){msg2}
			assertResult(idx){idx2}
		}
		case x => fail("Not a Failure: " + x)
	}
}
