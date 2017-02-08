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
import com.rayrobdod.json.union.ParserRetVal._

final class ParserRetValTest extends FunSpec {
	
	describe("ParserRetVal") {
		describe("map") {
			it ("is avaliable for Complex =:= Nothing") {
				assertResult(Primitive(6)){
					(Primitive(3)).map{x:Int => x * 2}
				}
			}
			it ("is avaliable for Primitive =:= Nothing") {
				assertResult(Complex(6)){
					(Complex(3)).map{x:Int => x * 2}
				}
			}
			it ("is not avaliable if neither success param is Nothing") {
				val x:ParserRetVal[Int, Int, Nothing, Nothing] = Complex(3)
				assertDoesNotCompile("""
					val x:ParserRetVal[Int, Int, Nothing, Nothing] = Complex(3)
					x.map{x => x * 2}
				""")
			}
		}
		describe("flatMap") {
			it ("is avaliable for Complex =:= Nothing") {
				assertResult(Complex(6)){
					(Primitive(3)).flatMap{x:Int => Complex(x * 2)}
				}
			}
			it ("is avaliable for Primitive =:= Nothing") {
				assertResult(Primitive(6)){
					(Complex(3)).flatMap{x:Int => Primitive(x * 2)}
				}
			}
			it ("is not avaliable if neither success param is Nothing") {
				val x:ParserRetVal[Int, Int, Nothing, Nothing] = Complex(3)
				assertDoesNotCompile("""
					val x:ParserRetVal[Int, Int, Nothing, Nothing] = Complex(3)
					x.flatMap{x => x * 2}
				""")
			}
		}
		describe("flip") {
			it ("Complex => Primitive") {
				assertResult(Complex(6)){Primitive(6).flip}
			}
			it ("is avaliable for Primitive =:= Nothing") {
				assertResult(Primitive(6)){Complex(6).flip}
			}
		}
		
	}
	
	describe("ComplexProjection") {
		describe("map") {
			it("Complex") { assertResult(Complex(12)){Complex(6).complex.map{x:Int => x * 2}} }
			it("Primitive") { assertResult(Primitive(3)){Primitive(3).complex.map{x:Int => x * 2}} }
			it("BuilderFailure") { assertResult(BuilderFailure("a")){BuilderFailure("a").complex.map{x:Int => x * 2}} }
			it("ParserFailure") { assertResult(ParserFailure("a")){ParserFailure("a").complex.map{x:Int => x * 2}} }
		}
		describe("flatmap") {
			it("Complex") { assertResult(Primitive(12)){Complex(6).complex.flatMap{x:Int => Primitive(x * 2)}} }
			it("Primitive") { assertResult(Primitive(3)){Primitive(3).complex.flatMap{x:Int => Primitive(x * 2)}} }
			it("BuilderFailure") { assertResult(BuilderFailure("a")){BuilderFailure("a").complex.flatMap{x:Int => Primitive(x * 2)}} }
			it("ParserFailure") { assertResult(ParserFailure("a")){ParserFailure("a").complex.flatMap{x:Int => Primitive(x * 2)}} }
		}
	}
}	
