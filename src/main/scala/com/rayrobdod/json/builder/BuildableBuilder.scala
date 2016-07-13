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
import scala.util.{Try, Success, Failure}
import com.rayrobdod.json.parser.Parser
import com.rayrobdod.json.union.ParserRetVal

/**
 * A Builder which can be built piecewise.
 * 
 * TODO: mention how
 * 
 * @since 3.0
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
	
	/** add a buildableBuilder that will be used upon recieving the given key */
	def addDef(key:Key, fun:BuildableBuilder.KeyDef[Key, Value, Subject]):BuildableBuilder[Key, Value, Subject] = {
		this.copy(keyDefs = this.keyDefs + ((key, fun)))
	}
	/** Change the defaultKeyDef to one that will pass subject through */
	def ignoreUnknownKeys:BuildableBuilder[Key, Value, Subject] = {
		this.copy(defaultKeyDef = BuildableBuilder.ignoreKeyDef[Key, Value, Subject])
	}
	
	
	/** @see Builder#apply */
	override def apply[Input](folding:Subject, key:Key, input:Input, parser:Parser[Key, Value, Input]):Either[(String, Int), Subject] = {
		keyDefs.getOrElse(key, defaultKeyDef).apply(folding, input, parser)
	}
}

/**
 * @since 3.0
 */
object BuildableBuilder{
	private[this] val unexpectedValueErrorMessage:Function1[Any, Left[(String, Int), Nothing]] = {x => Left("Unexpected value: " + x, 0)}
	
	/**
	 * A holder for a Function3 that is allowed to have a variable type parameter
	 * @since 3.0
	 */
	abstract class KeyDef[Key, Value, Subject] {
		/** add a key-value pair to `s`; where `p.parse(someBuilder, i)` is the value, and the key is hard-coded. */
		def apply[Input](s:Subject, i:Input, p:Parser[Key, Value, Input]):Either[(String, Int), Subject]
	}
	
	/**
	 * A KeyDef that is partitioned into a set of component functions
	 * 
	 * @since 3.0
	 * @param builder the builder that handles input.
	 * @param convert convert a builder result into a value usable by fold. This is a partial function;
	 *       anything not defined by this function is turned into an error value.
	 * @param fold combine the previous subject and a successful convert into a new subject.
	 */
	def partitionedKeyDef[Key, Value, Subject, BuilderResult, MiddleType](
		builder:Builder[Key, Value, BuilderResult],
		convert:PartialFunction[ParserRetVal[BuilderResult, Value], Either[(String, Int), MiddleType]],
		fold:Function2[Subject, MiddleType, Subject]
	):KeyDef[Key, Value, Subject] = new KeyDef[Key, Value, Subject]{
		def apply[Input](folding:Subject, input:Input, parser:Parser[Key, Value, Input]):Either[(String, Int), Subject] = {
			val parserRetVal = parser.parse(builder, input)
			if (convert.isDefinedAt(parserRetVal)) {
				convert(parserRetVal).right.map{x:MiddleType => fold(folding, x)}
			} else {
				parserRetVal.fold(unexpectedValueErrorMessage, unexpectedValueErrorMessage, {(s, i) => Left(s,i)})
			}
		}
	}
	
	/**
	 * A KeyDef that is partitioned into a set of component functions
	 * 
	 * @since 3.0
	 * @param builder the builder that handles input.
	 * @param fold combine the previous subject and a successful convert into a new subject.
	 */
	def partitionedComplexKeyDef[Key, Value, Subject, BuilderResult](
		builder:Builder[Key, Value, BuilderResult],
		fold:Function2[Subject, BuilderResult, Either[(String, Int), Subject]]
	):KeyDef[Key, Value, Subject] = new KeyDef[Key, Value, Subject]{
		def apply[Input](folding:Subject, input:Input, parser:Parser[Key, Value, Input]):Either[(String, Int), Subject] = {
			parser.parse(builder, input)
				.fold(
					{x => Right(x)},
					unexpectedValueErrorMessage,
					{(s,i) => Left(s,i)}
				)
				.right.flatMap{x:BuilderResult => fold(folding, x)}
		}
	}
	
	/**
	 * A KeyDef that is partitioned into a set of component functions
	 * 
	 * @since 3.0
	 * @param convert convert a builder result into a value usable by fold. This is a partial function;
	 *       anything not defined by this function is turned into an error value.
	 * @param fold combine the previous subject and a successful convert into a new subject.
	 */
	def partitionedPrimitiveKeyDef[Key, Value, Subject, MiddleType](
		convert:PartialFunction[Value, Either[(String, Int), MiddleType]],
		fold:Function2[Subject, MiddleType, Subject]
	):KeyDef[Key, Value, Subject] = new KeyDef[Key, Value, Subject]{
		def apply[Input](folding:Subject, input:Input, parser:Parser[Key, Value, Input]):Either[(String, Int), Subject] = {
			parser.parsePrimitive(input)
				.right.flatMap{value => (if (convert.isDefinedAt(value)) { convert.apply(value) } else { unexpectedValueErrorMessage(value) } )}
				.right.map{x:MiddleType => fold(folding, x)}
		}
	}
	
	/** 
	 * A KeyDef that simply passes through the subject
	 * @since 3.0
	 */
	def ignoreKeyDef[K,V,A]:KeyDef[K,V,A] = new KeyDef[K,V,A]{
		def apply[Input](s:A, i:Input, p:Parser[K,V,Input]):Either[(String, Int), A] = Right(s)
	}
	
	/**
	 * A KeyDef that throws an exception
	 * @since 3.0
	 */
	def throwKeyDef[K,V,A]:KeyDef[K,V,A] = new KeyDef[K,V,A]{
		def apply[Input](s:A, i:Input, p:Parser[K,V,Input]):Either[(String, Int), A] = Left("BuildableBuilder has no KeyDef for given key", 0)
	}
}
