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

import java.text.ParseException
import com.rayrobdod.json.parser.Parser
import scala.collection.immutable.Seq;
import scala.util.{Try, Success, Failure}

/** 
 * A Builder that will build a Vector of values, where each inner value was c
 * 
 * @version next
 * @tparam Key the type of keys used by the Parser that this Builder will be used by
 * @tparam Value the type of primitive value types used by the Parser that this Builder will be used by
 * @constructor
 * A builder that will create seqs of values built with the specified childbuilder
 * @param childBuilder the type of this seq's complex child elements. If it is Nothing, it will default to making more SeqBuilders
 */
final class SeqBuilder[Key, Value, Inner](childBuilder:Builder[Key, Value, Inner]) extends Builder[Key, Value, Seq[Inner]] {
	
	def init:Seq[Inner] = Vector.empty[Inner]
	
	override def apply[Input](folding:Seq[Inner], key:Key, innerInput:Input, parser:Parser[Key, Value, Input]):Try[Seq[Inner]] = {
		val res = parser.parse(childBuilder, innerInput)
		res match {
			case Success(Left(x)) => Try(folding :+ x)
			case Success(Right(x)) => Failure(new ParseException("Found primitive in SeqBuilder", 0))
			case Failure(x) => Failure(x)
		}
	}
}

/**
 * 
 * @since next
 */
final class PrimitiveSeqBuilder[Key, Value] extends Builder[Key, Value, Seq[Value]] {
	def init:Seq[Value] = Vector.empty[Value]
	
	override def apply[Input](folding:Seq[Value], key:Key, innerInput:Input, parser:Parser[Key, Value, Input]):Try[Seq[Value]] = {
		val res = parser.parse(new ThrowBuilder(), innerInput)
		res match {
			case Success(Left(x)) => Failure(new ParseException("Found complex in SeqBuilder", 0))
			case Success(Right(x)) => Try(folding :+ x)
			case Failure(x) => Failure(x)
		}
	}
}
