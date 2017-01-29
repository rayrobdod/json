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

import shapeless.{HList, ::, HNil, Witness, LabelledGeneric}
import shapeless.labelled.FieldType
import com.rayrobdod.json.builder.Builder
import com.rayrobdod.json.union.ParserRetVal

/**
 * A builder which can parse case classes.
 * 
 * @since next
 * @tparam Value the type of primitive value types used by the Parser that this Builder will be used by
 * @tparam Input the type of object parsed by this Parser
 */
final class ShapelessParser[Value, Input](implicit folder:ShapelessParser.Folder[Value, Input]) extends Parser[String, Value, Input] {
	override def parse[ComplexOutput](builder:Builder[String, Value, ComplexOutput], i:Input):ParserRetVal[ComplexOutput, Value] = {
		ParserRetVal.eitherToComplex(folder.parse(builder, i))
	}
}

/**
 * Implicits to be pulled in by ShapelessParser
 * 
 * @since next
 */
object ShapelessParser {
	/**
	 * A slightly simplified Parser that rejects the possibility of returning a ParserRetVal.Primitive
	 */
	trait Folder[+Value, -Input] {
		def parse[ComplexOutput](builder:Builder[String, Value, ComplexOutput], i:Input):Either[(String, Int), ComplexOutput]
	}
	
	
	private[this] object HnilFolder extends Folder[Nothing, HNil]{
		override def parse[ComplexOutput](builder:Builder[String, Nothing, ComplexOutput], i:HNil):Either[(String, Int), ComplexOutput] = {
			Right(builder.init)
		}
	}
	/** A fallback Folder which reports that the key is unexpected */
	implicit def hnilFolder[Value]:Folder[Value, HNil] = HnilFolder
	
	implicit def hlistFolder[Value, K <: Symbol, H, T <: HList](
		implicit
		  witness: Witness.Aux[K]
		, hParser: Parser[String, Value, H]
		, tEncoder: Folder[Value, T]
	):Folder[Value, FieldType[K, H] :: T] = new Folder[Value, FieldType[K, H] :: T]{
		def parse[ComplexOutput](builder:Builder[String, Value, ComplexOutput], input:FieldType[K, H] :: T):Either[(String, Int), ComplexOutput] = {
			tEncoder.parse(builder, input.tail).right.flatMap{x =>
				val key = witness.value.name
				val value = input.head
				
				builder.apply(x, key, value, hParser)
			}
		}
	}
	
	implicit def hlistImplFolder[Value, Input, InputRepr](
		implicit
		  generic:LabelledGeneric.Aux[Input, InputRepr]
		, reprEncoder:Folder[Value, InputRepr]
	):Folder[Value, Input] = new Folder[Value, Input]{
		def parse[ComplexOutput](builder:Builder[String, Value, ComplexOutput], input:Input):Either[(String, Int), ComplexOutput] = {
			reprEncoder.parse(builder, generic.to(input))
		}
	}
	
	implicit def identParser[V]:Parser[String, V, V] = new IdentityParser[V]
	implicit def identParserWithMap[V,S](implicit transform:S => V):Parser[String, V, S] = new IdentityParser[S].mapValue(transform)
	

}