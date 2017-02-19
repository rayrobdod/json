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

import scala.util.Either
import com.rayrobdod.json.builder.Builder
import com.rayrobdod.json.union.ParserRetVal
import com.rayrobdod.json.union.Failures.{ExpectedPrimitive}

/**
 * An object that parses an input into a sequence of key-value pairs for the
 * purpose of inserting those key-value pairs into a Builder
 * 
 * @see [[com.rayrobdod.json.builder.Builder]]
 * @since 3.0
 * @version 4.0
 * @tparam Key the key types
 * @tparam Value the primitive value types
 * @tparam Failure possible ways this parser can fail
 * @tparam Annotation annotations that this adds to builder failures
 * @tparam Input the input to the parser
 */
trait Parser[+Key, +Value, +Failure, -Input] {
	
	/**
	 * Convert the input into a series of key-value pairs, insert those key-value pairs into `builder`, return the value output by `builder`
	 * 
	 * @param builder a builder that consumes key-value pairs and produced a complex value. 
	 * @param i the input to the parser - the implementation of parser determines what it uses as input
	 * @return
		- An error containing a message and index into i
		- A primitive value
		- A complex value produced by feeding key-value pairs to the builder
	 * @tparam ComplexOutput the type of object the Builder produces
	 */
	def parse[ComplexOutput, BuilderFailure](builder:Builder[Key, Value, BuilderFailure, ComplexOutput], i:Input):ParserRetVal[ComplexOutput, Value, Failure, BuilderFailure]
	
	/**
	 * Parse the input into a Value. Return a Right if [[Parser.parse]] would have
	 * returned a [[com.rayrobdod.json.union.ParserRetVal.Primitive Primitive]], else return a Left.
	 */
	final def parsePrimitive(i:Input):ParserRetVal[Nothing, Value, Failure, ExpectedPrimitive.type] = {
		val ignoreAllBuilder = new Builder[Key, Value, Nothing, Any] {
			def init:Any = this
			def apply[I,BF](a:Any,k:Key,i:I,p:Parser[Key,Value,BF,I]):ParserRetVal.Complex[Any] = ParserRetVal.Complex(a)
		}
		
		this.parse(ignoreAllBuilder, i).complex.flatMap{x => (ParserRetVal.BuilderFailure(ExpectedPrimitive))}
	}
	
	
	/** Change the type of key that this builder requires */
	final def mapKey[K2](implicit fun:Function1[Key,K2]):Parser[K2,Value,Failure,Input] = new Parser[K2,Value,Failure,Input] {
		override def parse[Output, BF](builder:Builder[K2,Value,BF,Output], i:Input):ParserRetVal[Output, Value, Failure, BF] = {
			Parser.this.parse(builder.mapKey[Key](fun), i)
		}
	}
	
	/** Change the type of key that this builder requires, with the option of indicating an error condition
	 * @since 3.1
	 */
	final def flatMapKey[K2,Err](fun:Function1[Key,Either[Err,K2]]):Parser[K2,Value,util.Either[Err,Failure],Input] = new Parser[K2,Value,util.Either[Err,Failure],Input] {
		override def parse[Output, BF](builder:Builder[K2,Value,BF,Output], i:Input):ParserRetVal[Output, Value, util.Either[Err,Failure], BF] = {
			Parser.this.parse(builder.flatMapKey[Key, Err](fun), i)
				.parserFailure.map(util.Right.apply _)
				.builderFailure.flatMap{_.fold(
					{x:Err => ParserRetVal.ParserFailure(util.Left(x))},
					{x:BF => ParserRetVal.BuilderFailure(x)}
				)}
		}
	}
	
	/** Change the type of value that this builder requires */
	final def mapValue[V2](implicit fun:Function1[Value,V2]):Parser[Key,V2,Failure,Input] = new Parser[Key,V2,Failure,Input] {
		override def parse[Output, BF](builder:Builder[Key,V2,BF,Output], i:Input):ParserRetVal[Output, V2, Failure, BF] = {
			Parser.this.parse[Output, BF](builder.mapValue[Value](fun), i).primitive.map(fun)
		}
	}
	
	/**
	 * Change the type of value that this builder requires, with the option of indicating an error condition
	 * @version 4.0
	 */
	final def flatMapValue[V2,Err](fun:Function1[Value,Either[Err,V2]]):Parser[Key,V2,util.Either[Err,Failure],Input] = new Parser[Key,V2,util.Either[Err,Failure],Input] {
		override def parse[Output, BF](builder:Builder[Key,V2,BF,Output], i:Input):ParserRetVal[Output, V2, util.Either[Err,Failure], BF] = {
			Parser.this.parse(builder.flatMapValue[Value, Err](fun), i)
				.parserFailure.map{util.Right.apply}
				.primitive.flatMap{pe => fun(pe).fold(
					{x:Err => ParserRetVal.ParserFailure(util.Left(x))},
					{x:V2 => ParserRetVal.Primitive(x)}
				)}
				.builderFailure.flatMap{_.fold(
					{x:Err => ParserRetVal.ParserFailure(util.Left(x))},
					{x:BF => ParserRetVal.BuilderFailure(x)}
				)}
		}
	}
	
	/**
	 * Change the type of failure that this parser produces
	 * @since 4.0
	 */
	final def mapFailure[F2](fun:Function1[Failure, F2]):Parser[Key,Value,F2,Input] = new Parser[Key,Value,F2,Input] {
		override def parse[Output, BF](builder:Builder[Key,Value,BF,Output], i:Input):ParserRetVal[Output, Value, F2, BF] = {
			Parser.this.parse(builder, i)
				.parserFailure.map(fun)
		}
	}
	
}
