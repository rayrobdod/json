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
package com.rayrobdod.json.builder;

import scala.beans.BeanProperty;
import scala.util.{Try, Success, Failure}
import java.text.ParseException;
import scala.collection.immutable.Seq;
import org.scalatest.FunSpec;
import com.rayrobdod.json.parser.FailureParser
import com.rayrobdod.json.parser.IdentityParser
import com.rayrobdod.json.parser.SeqParser
import com.rayrobdod.json.parser.PrimitiveSeqParser
import com.rayrobdod.json.parser.CaseClassParser

class BuilderTest extends FunSpec {
	
	describe("Builder.mapKey") {
	}
	describe("Builder.mapValue") {
		it ("passes through a parser's falure") {
			val myValue2 = new Object
			
			assertFailure(classOf[NoSuchElementException]){
				new SeqBuilder(new PrimitiveSeqBuilder[String, Object]).mapValue[Object].apply(Nil, "sdfa", myValue2, new FailureParser(new NoSuchElementException))
			}
		}
	}
	
	
	
	def assertFailure[T](clazz:Class[T])(result:Try[_]):Unit = result match {
		case Failure(x) => {
			if (! clazz.isInstance(x)) {
				fail("Wrong type of failure: " + x)
			}
		}
		case x => fail("Not a Failure: " + x)
	}
}

