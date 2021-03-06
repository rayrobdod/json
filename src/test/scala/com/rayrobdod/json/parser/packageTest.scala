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

class packageTest extends FunSpec {
	describe("Iterator2Reader") {
		it ("full read") {
			val buf = new Array[Char](8)
			val count = new Iterator2Reader("12345".iterator).read(buf)
			assertResult(Array('1', '2', '3', '4', '5', '\u0000', '\u0000', '\u0000')){buf}
			assertResult(5){count}
		}
		it ("partial read") {
			val buf = new Array[Char](8)
			val count = new Iterator2Reader("12345".iterator).read(buf, 2, 3)
			assertResult(Array('\u0000', '\u0000', '1', '2', '3', '\u0000', '\u0000', '\u0000')){buf}
			assertResult(3){count}
		}
		it ("close") {
			new Iterator2Reader("12345".iterator).close()
		}
	}
	describe("CountingReader") {
		describe ("goBackOne") {
			it ("throws if called twice consecutively") {
				intercept[IllegalStateException] {
					val dut = new CountingReader(new java.io.StringReader("adf"))
					dut.read()
					dut.goBackOne()
					dut.goBackOne()
				}
			}
			it ("causes the previously read character to be output upon the next read") {
				val dut = new CountingReader(new java.io.StringReader("adf"))
				assertResult('a'){dut.read()}
				dut.goBackOne()
				assertResult('a'){dut.read()}
			}
		}
	}
}
