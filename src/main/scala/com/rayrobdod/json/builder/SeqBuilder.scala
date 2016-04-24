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
package com.rayrobdod.json.builder;

import java.text.ParseException
import com.rayrobdod.json.parser.Parser
import scala.collection.immutable.Seq;

/** A builder that creates seqs
 * 
 * @constructor
 * A builder that will create seqs, where complex types are built by `childBuilder.getOrElse(this)`
 * @param childBuilder the type of this seq's complex child elements. If it is Nothing, it will default to making more SeqBuilders
 */
final class SeqBuilder[Key, Value, Inner](childBuilder:Builder[Key, Value, Inner]) extends Builder[Key, Value, Seq[Inner]] {
	
	def init:Seq[Inner] = Vector.empty[Inner]
	
	def apply[Input](key:Key):Function3[Seq[Inner], Input, Parser[Key, Value, Input], Seq[Inner]] = {(folding, innerInput, parser) =>
		val res = parser.parse(childBuilder, innerInput)
		res match {
			case Left(x) => folding :+ x
			case Right(x) => throw new ParseException("Found primitive in SeqBuilder", 0)
		}
	}
}

/**
 * 
 */
final class PrimitiveSeqBuilder[Key, Value] extends Builder[Key, Value, Seq[Value]] {
	def init:Seq[Value] = Vector.empty[Value]
	
	def apply[Input](key:Key):Function3[Seq[Value], Input, Parser[Key, Value, Input], Seq[Value]] = {(folding, innerInput, parser) =>
		val res = parser.parse(new ThrowBuilder(), innerInput)
		res match {
			case Left(x) => throw new ParseException("Found complex in SeqBuilder", 0)
			case Right(x) => folding :+ x
		}
	}
}
