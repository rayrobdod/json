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
 * A parser that reads each key-value pair from a Map
 * @version 4.0
 * 
 * @tparam K the type of keys contained in the Map
 * @tparam V the type of values contained in the Map
 * @constructor
 * Create a MapParser
 */
final class MapParser[K,V] extends Parser[K,V,Nothing,Map[K,V]] {
	/**
	 * Decodes the input values to an object.
	 * @param vals the sequence containing values
	 * @return the parsed object
	 */
	def parse[A,BF](topBuilder:Builder[K,V,BF,A], vals:Map[K, V]):ParserRetVal[A, Nothing, Nothing, BF] = {
		vals.foldLeft[ParserRetVal[A, Nothing, Nothing, BF]](Complex(topBuilder.init)){(state:ParserRetVal[A, Nothing, Nothing, BF], keyValue:(K, V)) => 
			val (key, value) = keyValue;
			state.complex.flatMap{x => topBuilder.apply(x, key, value, new IdentityParser[V])}
		}
	}
}

/**
 * A parser that can parse the results of recursive MapBuilder builds
 * @version 4.0
 * @tparam K the type of keys contained in the Map
 * @tparam V the primitive values contained in the Map
 * TODO make not-private in future version
 */
private[parser] final class RecursiveMapParser[K,V] extends Parser[K, V, Nothing, com.rayrobdod.json.builder.MapBuilder.RecursiveSubjectType[K,V]] {
	import com.rayrobdod.json.builder.MapBuilder
	type RecursiveSubjectTupleType[K,V] = Tuple2[K, Either[MapBuilder.RecursiveSubject[K, V], V]]
	
	/**
	 * Decodes the input values to an object.
	 * @param vals the sequence containing values
	 * @return the parsed object
	 */
	def parse[A,BF](topBuilder:Builder[K,V,BF,A], vals:MapBuilder.RecursiveSubjectType[K,V]):ParserRetVal[A, Nothing, Nothing, BF] = {
		vals.foldLeft[ParserRetVal[A, Nothing, Nothing, BF]](Complex(topBuilder.init)){(state:ParserRetVal[A, Nothing, Nothing, BF], keyValue:RecursiveSubjectTupleType[K,V]) => 
			val (key, value) = keyValue;
			state.complex.flatMap{folding =>
				value.fold({complex:MapBuilder.RecursiveSubject[K,V] =>
					topBuilder.apply(folding, key, complex.value, RecursiveMapParser.this)
				}, {simple =>
					topBuilder.apply(folding, key, simple, new IdentityParser[V])
				})
			}
		}
	}
}