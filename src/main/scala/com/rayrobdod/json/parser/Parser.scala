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
import com.rayrobdod.json.builder.ThrowBuilder
import com.rayrobdod.json.union.ParserRetVal
import com.rayrobdod.json.builder.PiecewiseBuilder.Failures.ExpectedPrimitive

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
 * @tparam BuilderFailureExtra extra data that this parser provides to Builder Failures
 */
trait Parser[+Key, +Value, +Failure, +BuilderFailureExtra, -Input] {
	
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
	def parse[ComplexOutput, BuilderFailure](
			builder:Builder[Key, Value, BuilderFailure, ComplexOutput], i:Input
	):ParserRetVal[ComplexOutput, Value, Failure, BuilderFailure, BuilderFailureExtra]
	
	/**
	 * Parse the input into a Value. Return a Right if [[Parser.parse]] would have
	 * returned a [[com.rayrobdod.json.union.ParserRetVal.Primitive Primitive]], else return a Left.
	 */
	@deprecated("This version doesn't allow specifying what the error is; use the two-arg version instead", "4.0")
	final def parsePrimitive(i:Input):ParserRetVal[Nothing, Value, Failure, ExpectedPrimitive.type, BuilderFailureExtra] = {
		this.parsePrimitive(i, ExpectedPrimitive)
	}
	
	/**
	 * Parse the input into a Value. Return a Right if [[Parser.parse]] would have
	 * returned a [[com.rayrobdod.json.union.ParserRetVal.Primitive Primitive]], else return a Left.
	 * @since 4.0
	 */
	final def parsePrimitive[BF](i:Input, bf:BF):ParserRetVal[Nothing, Value, Failure, BF, BuilderFailureExtra] = {
		this.parse(new ThrowBuilder(bf), i)
	}
	
	
	/** Change the type of key that this builder requires */
	final def mapKey[K2](implicit fun:Function1[Key,K2]):Parser[K2,Value,Failure,BuilderFailureExtra,Input] = new Parser[K2,Value,Failure,BuilderFailureExtra,Input] {
		override def parse[Output, BF](builder:Builder[K2,Value,BF,Output], i:Input):ParserRetVal[Output, Value, Failure, BF, BuilderFailureExtra] = {
			Parser.this.parse(builder.mapKey[Key](fun), i)
		}
	}
	
	/** Change the type of key that this builder requires, with the option of indicating an error condition
	 * @since 3.1
	 */
	final def flatMapKey[K2,Err](fun:Function1[Key,Either[Err,K2]]):Parser[K2,Value,util.Either[Err,Failure],BuilderFailureExtra,Input] = new Parser[K2,Value,util.Either[Err,Failure],BuilderFailureExtra,Input] {
		override def parse[Output, BF](builder:Builder[K2,Value,BF,Output], i:Input):ParserRetVal[Output, Value, util.Either[Err,Failure], BF, BuilderFailureExtra] = {
			Parser.this.parse(builder.flatMapKey[Key, Err](fun), i)
				.parserFailure.map(util.Right.apply _)
				.builderFailure.flatMap{(bf, extra) => bf.fold(
					{x:Err => ParserRetVal.ParserFailure(util.Left(x))},
					{x:BF => ParserRetVal.BuilderFailure(x, extra)}
				)}
		}
	}
	
	/**
	 * Change the type of key that this builder requires, with the option of indicating an error condition
	 * and without discarding the BuilderFailureExtra while doing so
	 * 
	 * The only reason to call this is if you are [[Builder::flatMapKey]]
	 */
	private[json] final def flatMapKeyWithoutDiscardingExtra[K2,Err,X2](
		fun:Function1[Key,Either[Err,K2]], mappingFailureExtra:X2
	)(implicit
		ev: BuilderFailureExtra <:< X2
	):Parser[K2,Value,util.Either[(Err,X2),Failure],BuilderFailureExtra,Input] = {
		new Parser[K2,Value,util.Either[(Err, X2),Failure],BuilderFailureExtra,Input] {
			override def parse[Output, BF](builder:Builder[K2,Value,BF,Output], i:Input):ParserRetVal[Output, Value, util.Either[(Err, X2),Failure], BF, BuilderFailureExtra] = {
				Parser.this.parse(builder.flatMapKey[Key, Err](fun), i)
					.parserFailure.map(util.Right.apply _)
					.builderFailure.flatMap{(bf, extra) => bf.fold(
						{x:Err => ParserRetVal.ParserFailure(util.Left((x, extra)))},
						{x:BF => ParserRetVal.BuilderFailure(x, extra)}
					)}
			}
		}
	}
	
	/** Change the type of value that this builder requires */
	final def mapValue[V2](implicit fun:Function1[Value,V2]):Parser[Key,V2,Failure,BuilderFailureExtra,Input] = new Parser[Key,V2,Failure,BuilderFailureExtra,Input] {
		override def parse[Output, BF](builder:Builder[Key,V2,BF,Output], i:Input):ParserRetVal[Output, V2, Failure, BF, BuilderFailureExtra] = {
			Parser.this.parse[Output, BF](builder.mapValue[Value](fun), i).primitive.map(fun)
		}
	}
	
	/**
	 * Change the type of value that this builder requires, with the option of indicating an error condition
	 * @version 4.0
	 */
	final def flatMapValue[V2,Err](fun:Function1[Value, Either[Err,V2]]):Parser[Key,V2,util.Either[Err,Failure],BuilderFailureExtra,Input] = new Parser[Key,V2,util.Either[Err,Failure],BuilderFailureExtra,Input] {
		override def parse[Output, BF](builder:Builder[Key,V2,BF,Output], i:Input):ParserRetVal[Output, V2, util.Either[Err,Failure], BF, BuilderFailureExtra] = {
			Parser.this.parse(builder.flatMapValue(fun), i)
				.parserFailure.map{util.Right.apply}
				.primitive.flatMap{pe => fun(pe).fold(
					{x:Err => ParserRetVal.ParserFailure(util.Left(x))},
					{x:V2 => ParserRetVal.Primitive(x)}
				)}
				.builderFailure.flatMap{(bf, extra) => bf.fold(
					{x:Err => ParserRetVal.ParserFailure(util.Left(x))},
					{x:BF => ParserRetVal.BuilderFailure(x, extra)}
				)}
		}
	}
	
	/**
	 * Change the type of value that this builder requires, with the option of indicating an error condition
	 * and without discarding the BuilderFailureExtra while doing so
	 * 
	 * The only reason to call this is if you are [[Builder::flatMapKey]]
	 */
	private[json] def flatMapValueWithoutDiscardingExtra[V2, Err, X2](
		fun:Function1[Value, Either[Err,V2]], mappingFailureExtra:X2
	)(implicit
		ev: BuilderFailureExtra <:< X2
	):Parser[Key,V2,util.Either[(Err,X2),Failure],BuilderFailureExtra,Input] = { 
		new Parser[Key,V2,util.Either[(Err,X2),Failure],BuilderFailureExtra,Input] {
			override def parse[Output, BF](builder:Builder[Key,V2,BF,Output], i:Input):ParserRetVal[Output, V2, util.Either[(Err,X2),Failure], BF, BuilderFailureExtra] = {
				Parser.this.parse(builder.flatMapValue(fun), i)
					.parserFailure.map{util.Right.apply}
					.primitive.flatMap{pe => fun(pe).fold(
						{x:Err => ParserRetVal.ParserFailure(util.Left((x, mappingFailureExtra)))},
						{x:V2 => ParserRetVal.Primitive(x)}
					)}
					.builderFailure.flatMap{(bf, extra) => bf.fold(
						{x:Err => ParserRetVal.ParserFailure(util.Left((x, ev(extra))))},
						{x:BF => ParserRetVal.BuilderFailure(x, extra)}
					)}
			}
		}
	}
	
	/**
	 * Change the type of failure that this parser produces
	 * @since 4.0
	 */
	final def mapFailure[F2](fun:Function1[Failure, F2]):Parser[Key,Value,F2,BuilderFailureExtra,Input] = new Parser[Key,Value,F2,BuilderFailureExtra,Input] {
		override def parse[Output, BF](builder:Builder[Key,Value,BF,Output], i:Input):ParserRetVal[Output, Value, F2, BF, BuilderFailureExtra] = {
			Parser.this.parse(builder, i)
				.parserFailure.map(fun)
		}
	}
	
	/**
	 * Change the type of extra that this parser produces
	 * @since 4.0
	 */
	final def mapExtra[E2](fun:Function1[BuilderFailureExtra, E2]):Parser[Key, Value, Failure, E2, Input] = new Parser[Key,Value,Failure,E2,Input] {
		override def parse[Output, BF](builder:Builder[Key,Value,BF,Output], i:Input):ParserRetVal[Output, Value, Failure, BF, E2] = {
			Parser.this.parse(builder, i)
				.builderFailure.map{(b,e) => ((b, fun(e)))}
		}
	}
	
}
