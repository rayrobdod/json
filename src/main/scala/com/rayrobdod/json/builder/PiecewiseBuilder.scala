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
import com.rayrobdod.json.union.ParserRetVal
import com.rayrobdod.json.union.ParserRetVal.{Complex, Failure}

/**
 * A Builder which can be built piecewise.
 * 
 * @example
 * {{{
 * case class Foo(a:String, b:Seq[JsonValue], c:String)
 * val fooBuilder = (new PiecewiseBuilder[StringOrInt, JsonValue, Foo](new Foo("", Seq.empty, ""))
 * 	.addDef(StringOrInt("a"), partitionedPrimitiveKeyDef({case JsonValueString(x) => Right(x)}, {(f:Foo, x:String) => f.copy(a = x)}))
 * 	.addDef(StringOrInt("c"), partitionedPrimitiveKeyDef({case JsonValueString(x) => Right(x)}, {(f:Foo, x:String) => f.copy(c = x)}))
 * 	.addDef(StringOrInt("b"), partitionedComplexKeyDef(new PrimitiveSeqBuilder[JsonValue], {(f:Foo, x:Seq[JsonValue]) => Right(f.copy(b = x))}))
 * )
 * 
 * val jsonParser = new JsonParser
 * jsonParser.parse(fooBuilder, """{"a":"","b":[]}""")
 * // results in `Foo("", Seq.empty, "")`
 * jsonParser.parse(fooBuilder, """{"a":"qwer","b":["z","x","c"],"c":"asdf"}""")
 * // results in `Foo("qwer", Seq(JsonValueString("z"), JsonValueString("x"),JsonValueString("c")), "asdf")`
 * }}}
 * 
 * @since 3.0
 * @version 4.0
 * @see Inspired by [[https://github.com/scopt/scopt/]]
 * 
 * @tparam Key the key types
 * @tparam Value the primitive value types
 * @tparam Subject the type of object to build
 * @constructor
 * @param init The starting point of the folding process
 * @param defaultKeyDef the KeyDef executed when keyDefs does not contain a KeyDef for a specified key
 * @param keyDefs the mapping of known keys to actual applies
 */
final class PiecewiseBuilder[Key, Value, Subject](
		val init:Subject,
		defaultKeyDef:PiecewiseBuilder.KeyDef[Key, Value, Subject] = PiecewiseBuilder.throwKeyDef[Key, Value, Subject],
		keyDefs:Map[Key, PiecewiseBuilder.KeyDef[Key, Value, Subject]] = Map.empty[Key, PiecewiseBuilder.KeyDef[Key, Value, Subject]]
) extends Builder[Key, Value, Subject] {
	
	/** add a KeyDef that will be used upon receiving the given key */
	def addDef(key:Key, fun:PiecewiseBuilder.KeyDef[Key, Value, Subject]):PiecewiseBuilder[Key, Value, Subject] = {
		new PiecewiseBuilder(init, defaultKeyDef, this.keyDefs + ((key, fun)))
	}
	/** Change the defaultKeyDef to one that will pass subject through */
	def ignoreUnknownKeys:PiecewiseBuilder[Key, Value, Subject] = {
		new PiecewiseBuilder(init, PiecewiseBuilder.ignoreKeyDef[Key, Value, Subject], keyDefs)
	}
	
	
	override def apply[Input](folding:Subject, key:Key, input:Input, parser:Parser[Key, Value, Input]):ParserRetVal[Subject, Nothing] = {
		keyDefs.getOrElse(key, defaultKeyDef).apply(folding, input, parser)
	}
}

/**
 * KeyDef and several implementations
 * @since 3.0
 * @version 4.0
 */
object PiecewiseBuilder{
	private[this] val unexpectedValueErrorMessage:Function1[Any, Failure] = {x => Failure("Unexpected value: " + x, 0)}
	
	/**
	 * A three-input function that accepts an object to build upon, and a input-parser pair that indicates a new value
	 * @since 3.0
	 * @version 4.0
	 */
	abstract class KeyDef[Key, Value, Subject] {
		/** add a key-value pair to `s`; where `p.parse(someBuilder, i)` is the value, and the key is hard-coded. */
		def apply[Input](s:Subject, i:Input, p:Parser[Key, Value, Input]):ParserRetVal[Subject, Nothing]
	}
	
	/**
	 * A KeyDef that is partitioned into a set of component functions
	 * 
	 * @since 3.0
	 * @version 4.0
	 * @param builder the builder that handles input.
	 * @param convert convert a builder result into a value usable by fold. This is a partial function;
	 *       anything not defined by this function is turned into an error value.
	 * @param fold combine the previous subject and a successful convert into a new subject.
	 */
	def partitionedKeyDef[Key, Value, Subject, BuilderResult, MiddleType](
		builder:Builder[Key, Value, BuilderResult],
		convert:PartialFunction[ParserRetVal[BuilderResult, Value], ParserRetVal[MiddleType, Nothing]],
		fold:Function2[Subject, MiddleType, Subject]
	):KeyDef[Key, Value, Subject] = new KeyDef[Key, Value, Subject]{
		def apply[Input](folding:Subject, input:Input, parser:Parser[Key, Value, Input]):ParserRetVal[Subject, Nothing] = {
			val parserRetVal = parser.parse(builder, input)
			if (convert.isDefinedAt(parserRetVal)) {
				convert(parserRetVal).complex.map{x:MiddleType => fold(folding, x)}
			} else {
				parserRetVal.fold(unexpectedValueErrorMessage, unexpectedValueErrorMessage, {(s, i) => Failure(s,i)})
			}
		}
	}
	
	/**
	 * A KeyDef that is partitioned into a set of component functions with the expectation of complex values
	 * 
	 * @since 3.0
	 * @version 4.0
	 * @param builder the builder that handles input.
	 * @param fold combine the previous subject and a successful convert into a new subject.
	 */
	def partitionedComplexKeyDef[Key, Value, Subject, BuilderResult](
		builder:Builder[Key, Value, BuilderResult],
		fold:Function2[Subject, BuilderResult, ParserRetVal[Subject, Nothing]]
	):KeyDef[Key, Value, Subject] = new KeyDef[Key, Value, Subject]{
		def apply[Input](folding:Subject, input:Input, parser:Parser[Key, Value, Input]):ParserRetVal[Subject, Nothing] = {
			parser.parse(builder, input)
				.fold(
					{x => Complex(x)},
					unexpectedValueErrorMessage,
					{(s,i) => Failure(s,i)}
				)
				.complex.flatMap{x:BuilderResult => fold(folding, x)}
		}
	}
	
	/**
	 * A KeyDef that is partitioned into a set of component functions with the expectation of primitive values
	 * 
	 * @since 3.0
	 * @version 4.0
	 * @param convert convert a builder result into a value usable by fold. This is a partial function;
	 *       anything not defined by this function is turned into an error value.
	 * @param fold combine the previous subject and a successful convert into a new subject.
	 */
	def partitionedPrimitiveKeyDef[Key, Value, Subject, MiddleType](
		convert:PartialFunction[Value, ParserRetVal[MiddleType, Nothing]],
		fold:Function2[Subject, MiddleType, Subject]
	):KeyDef[Key, Value, Subject] = new KeyDef[Key, Value, Subject]{
		def apply[Input](folding:Subject, input:Input, parser:Parser[Key, Value, Input]):ParserRetVal[Subject, Nothing] = {
			parser.parsePrimitive(input)
				.flip.mergeToComplex
				.complex.flatMap{value:Value => (if (convert.isDefinedAt(value)) { convert.apply(value) } else { unexpectedValueErrorMessage(value) } )}
				.complex.map{x:MiddleType => fold(folding, x)}
		}
	}
	
	/** 
	 * A KeyDef that returns the folding value unchanged
	 * @since 3.0
	 * @version 4.0
	 */
	def ignoreKeyDef[K,V,A]:KeyDef[K,V,A] = new KeyDef[K,V,A]{
		def apply[Input](s:A, i:Input, p:Parser[K,V,Input]):ParserRetVal[A, Nothing] = Complex(s)
	}
	
	/**
	 * A KeyDef that returns an error message
	 * @since 3.0
	 * @version 4.0
	 */
	def throwKeyDef[K,V,A]:KeyDef[K,V,A] = new KeyDef[K,V,A]{
		def apply[Input](s:A, i:Input, p:Parser[K,V,Input]):ParserRetVal[A, Nothing] = Failure("PiecewiseBuilder has no KeyDef for given key", 0)
	}
}
