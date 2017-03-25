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

import scala.collection.immutable.Seq
import com.rayrobdod.json.parser.Parser
import com.rayrobdod.json.union.ParserRetVal
import com.rayrobdod.json.union.ParserRetVal.{Complex, BuilderFailure}
import com.rayrobdod.json.union.Failures.{ExpectedPrimitive, ExpectedComplex}

/** 
 * A Builder that will build a Vector of values, where each inner value is produced by the parameter builder.
 * 
 * This builder ignores keys completely, and adds elements to the sequence in encounter order.
 * 
 * [[#apply]] will return a left if the value is a primitive value.
 * 
 * @version 4.0
 * @tparam Key the type of keys encountered
 * @tparam Value the type of primitive values encountered
 * @tparam Inner the type of complex values produced by the childBuilder
 * @constructor
 * A builder that will create seqs of values built with the specified child builder
 * @param childBuilder a builder that this will use to produce child elements
 */
final class SeqBuilder[-Key, -Value, Failure, Inner](childBuilder:Builder[Key, Value, Failure, Inner]) extends Builder[Key, Value, Either[ExpectedComplex.type, Failure], Seq[Inner]] {
	override type Middle = Seq[Inner]
	override def init:Seq[Inner] = Vector.empty[Inner]
	override def apply[Input, PF](folding:Seq[Inner], key:Key, innerInput:Input, parser:Parser[Key, Value, PF, Input]):ParserRetVal[Seq[Inner], Nothing, PF, Either[ExpectedComplex.type, Failure]] = {
		parser.parse(childBuilder, innerInput)
				.complex.map{x => folding :+ x}
				.builderFailure.map{util.Right.apply _}
				.primitive.flatMap{x => BuilderFailure(util.Left(ExpectedComplex))}
	}
	override def finalize(folding:Seq[Inner]):ParserRetVal.Complex[Seq[Inner]] = ParserRetVal.Complex(folding)
}

/**
 * A Builder that will build a Vector of values, where each inner value is a primitive value.
 * 
 * This builder ignores keys completely, and adds elements to the sequence in encounter order.
 * 
 * [[#apply]] will return a left if the value is a complex value. 
 * 
 * @since 3.0
 * @version 4.0
 * @tparam Value the type of primitive values encountered
 */
final class PrimitiveSeqBuilder[Value] extends Builder[Any, Value, ExpectedPrimitive.type, Seq[Value]] {
	override type Middle = Seq[Value]
	override def init:Seq[Value] = Vector.empty[Value]
	override def apply[Input, PF](folding:Seq[Value], key:Any, innerInput:Input, parser:Parser[Any, Value, PF, Input]):ParserRetVal[Seq[Value], Nothing, PF, ExpectedPrimitive.type] = {
		parser.parsePrimitive(innerInput).primitive.flatMap{x => Complex(folding :+ x)}.mergeToComplex
	}
	override def finalize(folding:Seq[Value]):ParserRetVal.Complex[Seq[Value]] = ParserRetVal.Complex(folding)
}
