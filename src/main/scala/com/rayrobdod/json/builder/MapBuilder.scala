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

import com.rayrobdod.json.parser.Parser
import com.rayrobdod.json.union.ParserRetVal
import scala.collection.immutable.Map

/**
 * A builder that creates maps.
 * 
 * @version 4.0
 * @constructor
 * Creates a MapBuilder which uses the specified key-to-MapChildBuilder function to create children
 * @param childBuilders A function that indicates which MapChildBuilder to use for a given key
 */
final class MapBuilder[K, V, F, Inner](childBuilders:Function1[K, MapBuilder.MapChildBuilder[K, V, F, _, Inner]]) extends Builder[K, V, F, Map[K, Either[Inner, V]]] {
	override type Middle = Map[K, Either[Inner, V]]
	override val init:Map[K, Either[Inner, V]] = Map.empty
	override def apply[Input, BF](folding:Map[K, Either[Inner, V]], key:K, innerInput:Input, parser:Parser[K, V, BF, Input]):ParserRetVal[Map[K, Either[Inner, V]], Nothing, BF, F] = {
		val childBuilder = childBuilders(key)
		childBuilder.apply(innerInput, parser).complex.map{eitherRes =>
			folding + (key -> eitherRes)
		}
	}
	override def finalize(folding:Map[K, Either[Inner, V]]):ParserRetVal.Complex[Map[K, Either[Inner, V]]] = ParserRetVal.Complex(folding)
}

/**
 * Holds MapChildBuilder and several MapBuilder factory methods
 * @since 3.0
 */
object MapBuilder {
	type RecursiveSubjectType[K,V] = Map[K, Either[RecursiveSubject[K, V], V]]
	private[json] def RecursiveSubjectType[K,V](
		vals:Tuple2[K, Either[RecursiveSubject[K, V], V]]*
	):Map[K, Either[RecursiveSubject[K, V], V]] = {
		Map.apply(vals:_*)
	}
	
	/**
	 * Types cannot be recursive without some kind of 'real' type in there somewhere
	 * @version 4.0
	 */
	final case class RecursiveSubject[K,V](val value:Map[K, Either[RecursiveSubject[K, V], V]])
	
	/**
	 * Pairs a builder and a function into a function to create a value from a parser and input.
	 * @version 4.0
	 */
	final class MapChildBuilder[K, V, F, A, Inner](builder:Builder[K, V, F, A], result:Function1[A, Inner]) {
		def apply[Input, PF](innerInput:Input, parser:Parser[K, V, PF, Input]):ParserRetVal[Either[Inner, V], Nothing, PF, F] = {
			parser.parse(builder, innerInput)
					.complex.map{s => Left(result(s))}
					.primitive.map{p => Right(p)}
					.mergeToComplex
		}
	}
	
	/** Creates a MapBuilder whose child builders are recursively map builders.
	 * @version 4.0
	 */
	def apply[K,V]:MapBuilder[K,V,Nothing,RecursiveSubject[K,V]] = new MapBuilder[K, V, Nothing, RecursiveSubject[K,V]]({x =>
		new MapChildBuilder[K, V, Nothing, Map[K,Either[RecursiveSubject[K,V],V]], RecursiveSubject[K,V]](MapBuilder.apply[K,V], {x:Map[K,Either[RecursiveSubject[K,V],V]] => new RecursiveSubject(x)})
	})
	
	/** Creates a MapBuilder which uses the specified Builder and an identity function to create children for all keys
	 * @version 4.0
	 */
	def apply[K,V,F,Inner](cb:Builder[K,V,F,Inner]):MapBuilder[K,V,F,Inner] = MapBuilder(new MapChildBuilder(cb, {x:Inner => x}))
	
	/** Creates a MapBuilder which uses the specified MapChildBuilder to create children for all keys
	 * @version 4.0
	 */
	def apply[K,V,F,Inner](cb:MapChildBuilder[K,V,F,_,Inner]):MapBuilder[K,V,F,Inner] = new MapBuilder({x => cb})
	
	/** Creates a MapBuilder which uses the key-to-builder function and an identity function to create children
	 * @version 4.0
	 */
	def apply[K,V,F,Inner](cbs:Function1[K, Builder[K,V,F,Inner]]):MapBuilder[K,V,F,Inner] = new MapBuilder({x => new MapChildBuilder(cbs(x), {x:Inner => x})}) 
	
	/** Creates a MapBuilder which uses the specified key-to-MapChildBuilder function to create children
	 * @version 4.0
	 * @param ev trivially true evidence because otherwise type erasure prevents this and other #apply(Function[_,_]):MapBuilder[_,_,_] from having the same name
	 */
	def apply[K,V,F,Inner](cbs:Function1[K, MapChildBuilder[K,V,F,_,Inner]])(implicit ev: Int =:= Int):MapBuilder[K,V,F,Inner] = new MapBuilder(cbs) 
}
