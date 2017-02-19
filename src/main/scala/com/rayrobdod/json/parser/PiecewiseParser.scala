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
import PiecewiseParser.KeyDef
import com.rayrobdod.json.union.PiecewiseBuilderFailures

/**
 * A parser which can be built piecewise
 * 
 * @example
 * {{{
 * case class Foo(a:String, b:Seq[String], c:String)
 * val fooParser = new PiecewiseParser[StringOrInt, String, Foo](
 * 	PiecewiseParser.primitiveKeyDef("a", {x => x.a}),
 * 	PiecewiseParser.complexKeyDef("b", {x => x.b}, new PrimitiveSeqParser[String].mapKey[StringOrInt]),
 * 	PiecewiseParser.optionalKeyDef(PiecewiseParser.primitiveKeyDef("c", {x => x.c}), {x => x.c != ""}) 
 * )
 * val jsonBuilder = new PrettyJsonBuilder(PrettyJsonBuilder.MinifiedPrettyParams).mapValue[String]
 * fooParser.parse(jsonbuilder, Foo("", Seq.empty, ""))
 * // results in `{"a":"","b":[]}`
 * fooParser.parse(jsonbuilder, Foo("qwer", Seq("z","x","c"), "asdf"))
 * // results in `{"a":"qwer","b":["z","x","c"],"c":"asdf"}`
 * }}}
 * 
 * @since 3.0
 * @version 4.0
 * @tparam Key the key types
 * @tparam Value the primitive value types
 * @tparam Input the input to the parser
 * @constructor
 * Create a PiecewiseParser from a set of commands
 * @param parts a sequence of key-value pairs to extract from the input
 */
// PiecewiseParser?
final class PiecewiseParser[+Key, +Value, -Input] (
		parts:KeyDef[Key,Value,Input]*
) extends Parser[Key,Value,PiecewiseBuilderFailures,Input] {
	
	override def parse[Output,BF](builder:Builder[Key,Value,BF,Output], i:Input):ParserRetVal[Output, Nothing, PiecewiseBuilderFailures, BF] = {
		parts.foldLeft[ParserRetVal[Output, Nothing, PiecewiseBuilderFailures, BF]](ParserRetVal.Complex(builder.init)){(state:ParserRetVal[Output, Nothing, PiecewiseBuilderFailures, BF], part:KeyDef[Key,Value,Input]) =>
			state.complex.flatMap{x => part.apply(builder, i, x)}
		}
	}
}

/**
 * KeyDef and implementations
 * @since 3.0
 */
object PiecewiseParser {
	
	/**
	 * A function which extracts a key-value pair form the input then, using
	 * [[com.rayrobdod.json.builder.Builder#apply]] inserts that key-value pair into the output.
	 * 
	 * This has to be extracted from PiecewiseParser to maintain type safety
	 * while maintaining the possibility of extracting a complex value.
	 * @version 4.0
	 */
	abstract class KeyDef[+Key, +Value, -Input] {
		def apply[Output,BF](builder:Builder[Key,Value,BF,Output], input:Input, currentOutput:Output):ParserRetVal[Output, Nothing, PiecewiseBuilderFailures, BF]
	}
	
	def optionalKeyDef[Key, Value, Input](backing:KeyDef[Key, Value, Input], filter:Input => Boolean):KeyDef[Key, Value, Input] = new KeyDef[Key, Value, Input]{
		def apply[Output,BF](builder:Builder[Key,Value,BF,Output], input:Input, currentOutput:Output):ParserRetVal[Output, Nothing, PiecewiseBuilderFailures, BF] = {
			if (filter(input)) {
				backing.apply(builder, input, currentOutput)
			} else {
				ParserRetVal.Complex(currentOutput)
			}
		}
	}
	
	def primitiveKeyDef[Key, Value, Input](key:Key, inputToValue:Function1[Input, Value]):KeyDef[Key, Value, Input] = {
		complexKeyDef(key, inputToValue, new IdentityParser[Value])
	}
	
	def complexKeyDef[Key, Primitive, Value, Input](key:Key, backing:Function1[Input,Value], parser:Parser[Key,Primitive,PiecewiseBuilderFailures,Value]):KeyDef[Key, Primitive, Input] = new KeyDef[Key,Primitive,Input]{
		def apply[Output, BF](builder:Builder[Key,Primitive,BF,Output], input:Input, currentOutput:Output):ParserRetVal[Output, Nothing, PiecewiseBuilderFailures, BF] = {
			val value = backing.apply(input)
			builder.apply(currentOutput, key, value, parser)
		}
	}
	
}
