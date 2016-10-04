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
import scala.collection.immutable.Map

/**
 * A builder that creates maps.
 * 
 * @version 3.0
 * @constructor
 * Creates a MapBuilder which uses the specified key-to-MapChildBuilder function to create children
 * @param childBuilders A function that indicates which MapChildBuilder to use for a given key
 */
final class MapBuilder[K, V, Inner](childBuilders:Function1[K, MapBuilder.MapChildBuilder[K, V, _, Inner]]) extends Builder[K, V, Map[K, Either[Inner, V]]] {
	override val init:Map[K, Either[Inner, V]] = Map.empty
	override def apply[Input](folding:Map[K, Either[Inner, V]], key:K, innerInput:Input, parser:Parser[K, V, Input]):Either[(String, Int), Map[K, Either[Inner, V]]] = {
		val childBuilder = childBuilders(key)
		childBuilder.apply(innerInput, parser).right.map{eitherRes =>
			folding + (key -> eitherRes)
		}
	}
}

/**
 * Holds MapChildBuilder and several MapBuilder factory methods
 * @since 3.0
 */
object MapBuilder {
	/**
	 * Pairs a builder and a function into a function to create a value from a parser and input.
	 */
	final class MapChildBuilder[K, V, A, Inner](builder:Builder[K, V, A], result:Function1[A, Inner]) {
		def apply[Input](innerInput:Input, parser:Parser[K, V, Input]):Either[(String, Int), Either[Inner, V]] = {
			parser.parse(builder, innerInput).fold({s => Right(Left(result(s)))}, {p => Right(Right(p))}, {(s,i) => Left((s,i))})
		}
	}
	
	/** Creates a MapBuilder whose child builders are recursively map builders. */
	def apply[K,V]:MapBuilder[K,V,Any] = new MapBuilder({x => new MapChildBuilder(MapBuilder.apply[K,V], {x:Any => x})})
	
	/** Creates a MapBuilder which uses the specified Builder and an identity function to create children for all keys */
	def apply[K,V,Inner](cb:Builder[K,V,Inner]):MapBuilder[K,V,Inner] = MapBuilder(new MapChildBuilder(cb, {x:Inner => x}))
	
	/** Creates a MapBuilder which uses the specified MapChildBuilder to create children for all keys */
	def apply[K,V,Inner](cb:MapChildBuilder[K,V,_,Inner]):MapBuilder[K,V,Inner] = new MapBuilder({x => cb})
	
	/** Creates a MapBuilder which uses the key-to-builder function and an identity function to create children */
	def apply[K,V,Inner](cbs:Function1[K, Builder[K,V,Inner]]):MapBuilder[K,V,Inner] = new MapBuilder({x => new MapChildBuilder(cbs(x), {x:Inner => x})}) 
	
	/** Creates a MapBuilder which uses the specified key-to-MapChildBuilder function to create children */
	/* different name because type erasure */
	def apply2[K,V,Inner](cbs:Function1[K, MapChildBuilder[K,V,_,Inner]]):MapBuilder[K,V,Inner] = new MapBuilder(cbs) 
}
