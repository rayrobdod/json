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
package com.rayrobdod.json.builder

import shapeless.{HList, ::, HNil, Witness, LabelledGeneric}
import shapeless.labelled.FieldType
import com.rayrobdod.json.parser.Parser
import com.rayrobdod.json.union.{JsonValue, CborValue}


/**
 * A builder which can build case classes.
 * 
 * Implemented using Shapeless, as opposed to the other one which used reflection.
 * 
 * @since next
 * @tparam Value the type of primitive value types used by the Parser that this Builder will be used by
 * @tparam Subject the type of object built by this Builder
 * @param init a Subject filled with default values
 * @param folder the implementation of this's apply method.
 */
final class ShapelessBuilder[Value, Subject](val init:Subject)(
	implicit folder:ShapelessBuilder.Folder[Value, Subject]
) extends Builder[String, Value, Subject] {
	def apply[Input](folding:Subject, key:String, input:Input,
			parser:Parser[String, Value, Input]):Either[(String, Int), Subject]
		= folder(folding, key, input, parser)
}


/**
 * Implicits to be pulled in by ShapelessBuilder
 * 
 * @since next
 */
object ShapelessBuilder {
	
	/**
	 * Converts a value (obtained by parsing the input using the parser) into a value of type `Result`
	 */
	trait Converter[-Primitive, +Result] {
		def apply[Input](input:Input, parser:Parser[String, Primitive, Input]):Either[(String, Int), Result]
	}
	
	/**
	 * An implementation of [[Builder#apply]] without an implementation of [[Builder#init]]
	 * @see [[Builder#apply]]
	 */
	trait Folder[-Value, Subject] {
		def apply[Input](folding:Subject, key:String, input:Input, parser:Parser[String, Value, Input]):Either[(String, Int), Subject]
	}
	
	
	private[this] object HnilFolder extends Folder[Any, HNil]{
		override def apply[Input](folding:HNil, key:String, input:Input, parser:Parser[String, Any, Input]):Either[(String, Int), HNil] = {
			Left((s"Unexpected key: $key", 0))
		}
	}
	/** A fallback Folder which reports that the key is unexpected */
	implicit def hnilFolder[Value]:Folder[Value, HNil] = HnilFolder
	
	implicit def hlistFolder[Value, K <: Symbol, H, T <: HList](
		implicit
		  witness: Witness.Aux[K]
		, hEncoder: Converter[Value, H]
		, tEncoder: Folder[Value, T]
	):Folder[Value, FieldType[K, H] :: T] = new Folder[Value, FieldType[K, H] :: T]{
		override def apply[Input](folding:FieldType[K, H] :: T, key:String, input:Input, parser:Parser[String, Value, Input]):Either[(String, Int), FieldType[K, H] :: T] = {
			if (key == witness.value.name) {
				hEncoder.apply(input, parser).right.map{x => x.asInstanceOf[FieldType[K, H]] :: (folding.tail)}
			} else {
				tEncoder.apply(folding.tail, key, input, parser).right.map{x => folding.head :: x}
			}
		}
	}
	
	implicit def hlistImplFolder[Value, Subject, SubjectRepr](
		implicit
		  generic:LabelledGeneric.Aux[Subject, SubjectRepr]
		, reprEncoder:Folder[Value, SubjectRepr]
	):Folder[Value, Subject] = new Folder[Value, Subject]{
		override def apply[Input](folding:Subject, key:String, input:Input, parser:Parser[String, Value, Input]):Either[(String, Int), Subject] = {
			reprEncoder.apply( generic.to(folding), key, input, parser ).right.map{generic.from _}
		}
	}
	
	
	/** A converter which does not alter a value */
	implicit def identityConverter[A]:Converter[A, A] = new Converter[A, A]{
		override def apply[Input](input:Input, parser:Parser[String, A, Input]):Either[(String, Int), A] = {
			parser.parsePrimitive(input)
		}
	}
	/** A converter which uses an in-implicit-scope builder when parsing a value */
	implicit def complexConverter[Value, Subject](implicit back:Builder[String, Value, Subject]):Converter[Value, Subject] = new Converter[Value, Subject]{
		override def apply[Input](input:Input, parser:Parser[String, Value, Input]):Either[(String, Int), Subject] = {
			parser.parse(back, input).complex.toEither
		}
	}
	
	
	
	implicit def jsonValueAsIfString:Converter[JsonValue, String] = new Converter[JsonValue, String]{
		override def apply[Input](input:Input, parser:Parser[String, JsonValue, Input]):Either[(String, Int), String] = {
			parser.parsePrimitive(input).right.flatMap{x => x.stringToEither{x => Right(x)}}
		}
	}
	implicit def jsonValueAsIfDouble:Converter[JsonValue, Double] = new Converter[JsonValue, Double]{
		override def apply[Input](input:Input, parser:Parser[String, JsonValue, Input]):Either[(String, Int), Double] = {
			parser.parsePrimitive(input).right.flatMap{x => x.numberToEither{x => Right(x.doubleValue)}}
		}
	}
	implicit def jsonValueAsIfBoolean:Converter[JsonValue, Boolean] = new Converter[JsonValue, Boolean]{
		override def apply[Input](input:Input, parser:Parser[String, JsonValue, Input]):Either[(String, Int), Boolean] = {
			parser.parsePrimitive(input).right.flatMap{x => x.booleanToEither{x => Right(x)}}
		}
	}
	implicit def jsonValueAsIfInteger:Converter[JsonValue, Int] = new Converter[JsonValue, Int]{
		override def apply[Input](input:Input, parser:Parser[String, JsonValue, Input]):Either[(String, Int), Int] = {
			parser.parsePrimitive(input).right.flatMap{x => x.integerToEither{x => Right(x)}}
		}
	}
	
	
	implicit def cborValueAsIfString:Converter[CborValue, String] = new Converter[CborValue, String]{
		override def apply[Input](input:Input, parser:Parser[String, CborValue, Input]):Either[(String, Int), String] = {
			parser.parsePrimitive(input).right.flatMap{x => x.stringToEither{x => Right(x)}}
		}
	}
	implicit def cborValueAsIfDouble:Converter[CborValue, Double] = new Converter[CborValue, Double]{
		override def apply[Input](input:Input, parser:Parser[String, CborValue, Input]):Either[(String, Int), Double] = {
			parser.parsePrimitive(input).right.flatMap{x => x.numberToEither{x => Right(x.doubleValue)}}
		}
	}
	implicit def cborValueAsIfBoolean:Converter[CborValue, Boolean] = new Converter[CborValue, Boolean]{
		override def apply[Input](input:Input, parser:Parser[String, CborValue, Input]):Either[(String, Int), Boolean] = {
			parser.parsePrimitive(input).right.flatMap{x => x.booleanToEither{x => Right(x)}}
		}
	}
	implicit def cborValueAsIfInteger:Converter[CborValue, Int] = new Converter[CborValue, Int]{
		override def apply[Input](input:Input, parser:Parser[String, CborValue, Input]):Either[(String, Int), Int] = {
			parser.parsePrimitive(input).right.flatMap{x => x.integerToEither{x => Right(x)}}
		}
	}
	
}
