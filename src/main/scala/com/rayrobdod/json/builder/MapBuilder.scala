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
import scala.collection.immutable.Map;
import scala.util.{Try, Success, Failure}

/** A builder that creates maps
 * 
 * @version next
 * @constructor
 * Create a MapBuilder instance
 * @param childBuilderMap a function pretty directly called by `childBuilder()`.
 *          By default, it is a function that creates more MapBuilders
 */
final class MapBuilder[K,V](childBuilders:Function1[K, Option[Builder[K, V, _]]] = MapBuilder.defaultChildBuilder[K,V]) extends Builder[K, V, Map[K, Any]] {
	override val init:Map[K, Any] = Map.empty
	override def apply[Input](folding:Map[K,Any], key:K, innerInput:Input, parser:Parser[K, V, Input]):Try[Map[K,Any]] = {
		val childBuilder = childBuilders(key).getOrElse(new ThrowBuilder())
		parser.parse(childBuilder, innerInput).map{eitherRes =>
			val innerRes:Any = eitherRes match {
				case Left(x) => x
				case Right(x) => x
			}
			
			folding + (key -> innerRes)
		}
	}
}

private object MapBuilder {
	def defaultChildBuilder[K,V]:Function1[K, Option[Builder[K, V, _]]] = {s:K => None}
}
