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

import scala.collection.immutable.Map
import com.rayrobdod.json.parser.Parser

/**
 * A Builder which can be built piecewise.
 * 
 * 
 * 
 * @see Inspired by [[https://github.com/scopt/scopt/]]
 * 
 * @tparam Key the key types
 * @tparam Value the primitive value types
 * @tparam Subject the type of object to build
 * @constructor
 * @param init The starting point of the folding process
 * @param defaultKeyDef the KeyDef executed when no other keys exist
 * @param keyDefs the mapping of known keys to actual applies
 */
final case class BuildableBuilder[Key, Value, Subject](
		val init:Subject,
		defaultKeyDef:BuildableBuilder.KeyDef[Key, Value, Subject] = BuildableBuilder.throwKeyDef[Key, Value, Subject],
		keyDefs:Map[Key, BuildableBuilder.KeyDef[Key, Value, Subject]] = Map.empty[Key, BuildableBuilder.KeyDef[Key, Value, Subject]]
) extends Builder[Key, Value, Subject] {
	
	def addDef(key:Key, fun:BuildableBuilder.KeyDef[Key, Value, Subject]):BuildableBuilder[Key, Value, Subject] = {
		this.copy(keyDefs = this.keyDefs + ((key, fun)))
	}
	/** Change the defaultKeyDef to one that will pass subject through */
	def ignoreUnknownKeys:BuildableBuilder[Key, Value, Subject] = {
		this.copy(defaultKeyDef = BuildableBuilder.ignoreKeyDef[Key, Value, Subject])
	}
	
	
	/** @see Builder#apply */
	override def apply[Input](key:Key, folding:Subject, input:Input, parser:Parser[Key, Value, Input]):Subject = {
		keyDefs.getOrElse(key, defaultKeyDef).apply(folding, input, parser)
	}
}

object BuildableBuilder{
	/**
	 * A holder for a Function3 that is allowed to have a variable type parameter
	 */
	abstract class KeyDef[Key, Value, Subject] {
		def apply[Input]:Function3[Subject, Input, Parser[Key, Value, Input], Subject]
	}
	
	/** A KeyDef that simply passes through the subject */
	def ignoreKeyDef[K,V,A]:KeyDef[K,V,A] = new KeyDef[K,V,A]{def apply[Input] = {(s,i,p) => s}}
	
	/** A KeyDef that throws an exception */
	def throwKeyDef[K,V,A]:KeyDef[K,V,A] = new KeyDef[K,V,A]{def apply[Input] = {(s,i,p) => throw new IllegalArgumentException("Unknown key")}}
}
