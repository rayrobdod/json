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

import com.rayrobdod.json.builder.Builder
import com.rayrobdod.json.union.ParserRetVal
import com.rayrobdod.json.union.ParserRetVal.Complex

/**
 * A parser that reads each Value and its index from a Seq
 * @version 4.0
 * 
 * @tparam V the type of values contained in the Seq
 * @constructor
 * Create a SeqParser
 */
final class PrimitiveSeqParser[V] extends Parser[Int,V,Nothing,Unit,Seq[V]] {
	/**
	 * Decodes the input values to an object.
	 * @param vals the sequence containing values
	 * @return the parsed object
	 */
	override def parse[A, BF](topBuilder:Builder[Int,V,BF,A], vals:Seq[V]):ParserRetVal[A, Nothing, Nothing, BF, Unit] = {
		vals.zipWithIndex.foldLeft[ParserRetVal[topBuilder.Middle, Nothing, Nothing, BF, Unit]](Complex(topBuilder.init)){(state, valueKey:(V, Int)) => 
			val (value, key) = valueKey;
			state.complex.flatMap{x => topBuilder.apply(x, key, value, new IdentityParser[V], ())}
		}.complex.flatMap{topBuilder.finish(())}
	}
}

object PrimitiveSeqParser {
	/**
	 * A PrimitiveSeqParser that parses `Seq[SeqValue]`, but with mappings to convert the native values into 
	 * 
	 * `new PrimitiveSeqParser[SeqValue].mapKey(keyMapping).mapValue(valueMapping)`
	 * @since 3.1
	 */
	def apply[BuilderKey, BuilderValue, SeqValue](
		implicit keyMapping : Int => BuilderKey, valueMapping : SeqValue => BuilderValue
	):Parser[BuilderKey, BuilderValue, Nothing, Unit, Seq[SeqValue]] = {
		new PrimitiveSeqParser[SeqValue].mapKey(keyMapping).mapValue(valueMapping)
	}
}

/**
 * A parser that reads and parses each Value and its index from a Seq
 * @version 4.0
 * 
 * @tparam K the type of key used by recurse
 * @tparam V the type of primitiveValue used by recurse
 * @tparam Inner the type of values contained in the Seq
 * @constructor
 * Create a SeqParser
 * @param recurse a parser for values contained in the sequence
 * @param keyMapping a mapping from integer indexies to type K.
 */
final class SeqParser[+K,+V,PF,BFE,-Inner](recurse:Parser[K,V,PF,BFE,Inner])(implicit keyMapping:Function1[Int, K], extraMapping:Function1[Unit, BFE]) extends Parser[K,V,PF,BFE,Seq[Inner]] {
	override def parse[A,BF](topBuilder:Builder[K,V,BF,A], vals:Seq[Inner]):ParserRetVal[A, Nothing, PF, BF, BFE] = {
		vals.zipWithIndex.foldLeft[ParserRetVal[topBuilder.Middle, Nothing, PF, BF, BFE]](Complex(topBuilder.init)){(state, valueKey:(Inner, Int)) => 
			val (value, key2) = valueKey
			val key = keyMapping(key2)
			state.complex.flatMap{x => topBuilder.apply(x, key, value, recurse, extraMapping(()))}
		}.complex.flatMap{topBuilder.finish(extraMapping(()))}
	}
}

object SeqParser {
}
