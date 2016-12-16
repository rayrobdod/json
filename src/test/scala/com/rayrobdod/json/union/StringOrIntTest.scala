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
package com.rayrobdod.json.union

import org.scalatest.FunSpec
import com.rayrobdod.json.union.StringOrInt._

class StringOrIntTest extends FunSpec {
	
	describe("StringOrInt.Left") {
		describe ("fold") {
			it ("uses the result of the left function") {
				assertResult("Left"){StringOrInt.Left("ASDF").fold({x => "Left"}, {x => "Right"})}
			}
			it ("provides the left's value to the function") {
				assertResult("ASDF Left"){StringOrInt.Left("ASDF").fold({x => x + " Left"}, {x => x + " Right"})}
			}
		}
	}
	describe("StringOrInt.Right") {
		describe ("fold") {
			it ("uses the result of the right function") {
				assertResult("Right"){StringOrInt.Right(1234).fold({x => "Left"}, {x => "Right"})}
			}
			it ("provides the left's value to the function") {
				assertResult("1234 Right"){StringOrInt.Right(1234).fold({x => x + " Left"}, {x => x + " Right"})}
			}
		}
	}
	
	describe("StringOrInt$") {
		describe("Implicits") {
			it ("""String to StringOrInt""") {
				val res:StringOrInt = "abx"
				assertResult(StringOrInt.Left("abx")){res}
			}
			it ("""StringOrInt.Right to JsonValue""") {
				val res:StringOrInt = 234
				assertResult(StringOrInt.Right(234)){res}
			}
		}
		describe("apply mehod") {
			it ("""String to StringOrInt""") {
				val res:StringOrInt = StringOrInt("abx")
				assertResult(StringOrInt.Left("abx")){res}
			}
			it ("""StringOrInt.Right to JsonValue""") {
				val res:StringOrInt = StringOrInt(234)
				assertResult(StringOrInt.Right(234)){res}
			}
		}
		describe("unwrapToString") {
			it ("String") {assertResult("abc"){unwrapToString(StringOrInt("abc"))}}
			it ("Int") {assertResult("123"){unwrapToString(StringOrInt(123))}}
		}
	}
}
