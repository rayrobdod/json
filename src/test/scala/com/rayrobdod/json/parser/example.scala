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
package com.rayrobdod.json.doc

import org.scalatest.FunSpec
import com.rayrobdod.json.union.ParserRetVal

final class Examples extends FunSpec {
	describe ("serializeExample") {
		import serializeExample._
		
		it ("result is ...") {
			val expected = ParserRetVal.Complex(
				"""{
  "name" : {
    "first" : "Anon",
    "middles" : [
      "N",
      "Y"
    ],
    "last" : "Mouse"
  },
  "gender" : "Undecided",
  "isAlive" : true,
  "interests" : [
    "Cheese",
    "Chess"
  ]
}"""
			)
			assertResult(expected){result}
		}
	}
	describe ("parsingExample") {
		import parsingExample._
		
		it ("result is ...") {
			val expected = ParserRetVal.Complex(
				Person(
					Name(
						"Raymond",
						"Robert",
						"Dodge"
					),
					"male",
					false,
					Seq(
						"bowling",
						"tennis",
						"programming",
						"twitch plays pokémon"
					)
				)
			)
			assertResult(expected){result}
		}
	}
}
