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
import com.rayrobdod.json.union.ParserRetVal

/**
 * An object which takes a series of key-value pairs and combines them into an object using
 * a series of fold-left style method calls.
 * 
 * @version 4.0
 * @see [[com.rayrobdod.json.parser.Parser]]
 * @tparam Key the type of keys used by the Parser that this Builder will be used by
 * @tparam Value the type of primitive value types used by the Parser that this Builder will be used by
 * @tparam Failure possible ways this builder can fail
 * @tparam Result the type of object built by this Builder
 */
trait Builder[-Key, -Value, +Failure, Result] {
	/**
	 * The type of object while folding.
	 * If `Builder` weren't so whitebox, this would be a hidden implementation detail.
	 * @since 4.0
	 */
	type Middle
	
	/**
	 * An 'empty' object. Can be thought of as either the value used at the
	 * start of a fold-left operation, or as an object filled with default values.
	 */
	def init:Middle
	
	/**
	 * Add a key-value pair to `folding`
	 * 
	 * The key is `key`, and the value is the result of [[com.rayrobdod.json.parser.Parser.parse]] on the provided `input`.
	 * 
	 * 
	 * @param folding the object to be added to. Must be either the return value of [[init]] or the return value of [[apply]]
	 * @param key the key of a key-value pair
	 * @param parser a parser for turning input into a value
	 * @param input the input to a parser
	 * @return either a built value or an error value
	 */
	def apply[Input, ParserFailure](folding:Middle, key:Key, input:Input, parser:Parser[Key, Value, ParserFailure, Input]):ParserRetVal[Middle, Nothing, ParserFailure, Failure]
	
	/**
	 * A function to be called after the folding to produce a result
	 *
	 * Taking advantage of this method might be useful if either `Middle` is
	 * mutable for performance reasons and `Result` should be immutable or
	 * if the builder needs to use multiple key-value pairs fromt the source
	 * to produce some value in the domain object.
	 *
	 * If there is no need to take advantage of this, the implementation of this
	 * method can be
	 * 
	 * {{{
	 * override def finish(x:Middle) = ParserRetVal.Complex(x)
	 * }}}
	 * 
	 * @since 4.0
	 * @param folding the object to be converted. Must be either the return value of [[init]] or the return value of [[apply]]
	 * @return either a built value or an error value
	 */
	def finish(folding:Middle):ParserRetVal[Result, Nothing, Nothing, Failure]
	
	/**
	 * Change the type of key that this builder requires
	 * @param fun a conversion function from the new key to this's key
	 * @since 3.0
	 */
	final def mapKey[K2](implicit fun:Function1[K2,Key]):Builder[K2,Value,Failure,Result] = new Builder[K2,Value,Failure,Result] {
		override type Middle = Builder.this.Middle
		override def init:Middle = Builder.this.init
		override def finish(folding:Middle):ParserRetVal[Result, Nothing, Nothing, Failure] = Builder.this.finish(folding)
		
		override def apply[Input, PF](a:Middle, key:K2, b:Input, c:Parser[K2, Value, PF, Input]):ParserRetVal[Middle, Nothing, PF, Failure] = {
			Builder.this.apply(a, fun(key), b, c.mapKey(fun))
		}
	}
	
	/**
	 * Change the type of key that this builder requires, with the option of indicating an error condition
	 * @param fun a conversion function from the new key to this's key
	 * @since 3.1
	 */
	final def flatMapKey[K2, Err](fun:Function1[K2,Either[Err,Key]]):Builder[K2,Value,util.Either[Err,Failure],Result] = new Builder[K2,Value,util.Either[Err,Failure],Result] {
		override type Middle = Builder.this.Middle
		override def init:Middle = Builder.this.init
		override def finish(folding:Middle):ParserRetVal[Result, Nothing, Nothing, scala.util.Either[Err,Failure]] = {
			Builder.this.finish(folding)
				.builderFailure.map(util.Right.apply _)
		}
		
		override def apply[Input, PF](a:Middle, key:K2, b:Input, c:Parser[K2, Value, PF, Input]):ParserRetVal[Middle, Nothing, PF, util.Either[Err,Failure]] = {
			fun(key).fold(
				{f => ParserRetVal.BuilderFailure(util.Left(f))},
				{k2:Key => Builder.this.apply(a, k2, b, c.flatMapKey(fun))
					.builderFailure.map{util.Right.apply}
					.parserFailure.flatMap{_.fold(
						{x => ParserRetVal.BuilderFailure(util.Left(x))},
						ParserRetVal.ParserFailure.apply _
					)}
				}
			)
		}
	}
	
	/**
	 * Change the type of value that this builder requires
	 * @param fun a conversion function from the new value to this's value
	 * @since 3.0
	 */
	final def mapValue[V2](implicit fun:Function1[V2,Value]):Builder[Key,V2,Failure,Result] = new Builder[Key,V2,Failure,Result] {
		override type Middle = Builder.this.Middle
		override def init:Middle = Builder.this.init
		override def finish(folding:Middle):ParserRetVal[Result, Nothing, Nothing, Failure] = Builder.this.finish(folding)
		
		override def apply[Input, PF](a:Middle, key:Key, b:Input, c:Parser[Key, V2, PF, Input]):ParserRetVal[Middle, Nothing, PF, Failure] = {
			Builder.this.apply(a, key, b, c.mapValue(fun))
		}
	}
	
	/**
	 * Change the type of value that this builder requires, with the option of indicating an error condition
	 * @since 3.0
	 */
	final def flatMapValue[V2,Err](fun:Function1[V2,Either[Err,Value]]):Builder[Key,V2,util.Either[Err,Failure],Result] = new Builder[Key,V2,util.Either[Err,Failure],Result] {
		override type Middle = Builder.this.Middle
		override def init:Middle = Builder.this.init
		override def finish(folding:Middle):ParserRetVal[Result, Nothing, Nothing, scala.util.Either[Err,Failure]] = {
			Builder.this.finish(folding)
				.builderFailure.map(util.Right.apply _)
		}
		
		override def apply[Input, PF](a:Middle, key:Key, b:Input, c:Parser[Key, V2, PF, Input]):ParserRetVal[Middle, Nothing, PF, util.Either[Err,Failure]] = {
			Builder.this.apply(a, key, b, c.flatMapValue(fun))
				.builderFailure.map(util.Right.apply _)
				.parserFailure.flatMap{_.fold(
					{x => ParserRetVal.BuilderFailure(util.Left(x))},
					ParserRetVal.ParserFailure.apply _
				)}
		}
	}
	
	/**
	 * Change the type of failure produced by this builder
	 * @since 4.0
	 */
	final def mapFailure[F2](fun:Function1[Failure,F2]):Builder[Key,Value,F2,Result] = new Builder[Key,Value,F2,Result] {
		override type Middle = Builder.this.Middle
		override def init:Middle = Builder.this.init
		override def finish(folding:Middle):ParserRetVal[Result, Nothing, Nothing, F2] = {
			Builder.this.finish(folding)
				.builderFailure.map(fun)
		}
		
		override def apply[Input, PF](a:Middle, key:Key, b:Input, c:Parser[Key, Value, PF, Input]):ParserRetVal[Middle, Nothing, PF, F2] = {
			Builder.this.apply(a, key, b, c)
				.builderFailure.map(fun)
		}
	}
	
	/**
	 * Change the type of result produced by this builder
	 * @since 4.0
	 */
	final def mapResult[R2](fun:Function1[Result,R2]):Builder[Key,Value,Failure,R2] = new Builder[Key,Value,Failure,R2] {
		override type Middle = Builder.this.Middle
		override def init:Middle = Builder.this.init
		override def apply[Input, PF](a:Middle, key:Key, b:Input, c:Parser[Key, Value, PF, Input]):ParserRetVal[Middle, Nothing, PF, Failure] = Builder.this.apply(a, key, b, c)
		
		override def finish(folding:Middle):ParserRetVal[R2, Nothing, Nothing, Failure] = {
			Builder.this.finish(folding).complex.map(fun)
		}
	}
	
	/**
	 * Change the type of result produced by this builder, with the option of indicating an error condition
	 * @since 4.0
	 */
	final def flatMapResult[R2,Err](fun:Function1[Result,Either[Err,R2]]):Builder[Key,Value,util.Either[Err,Failure],R2] = new Builder[Key,Value,util.Either[Err,Failure],R2] {
		override type Middle = Builder.this.Middle
		override def init:Middle = Builder.this.init
		override def apply[Input, PF](a:Middle, key:Key, b:Input, c:Parser[Key, Value, PF, Input]):ParserRetVal[Middle, Nothing, PF, util.Either[Err,Failure]] = {
			Builder.this.apply(a, key, b, c)
				.builderFailure.map(util.Right.apply _)
		}
		
		override def finish(folding:Middle):ParserRetVal[R2, Nothing, Nothing, util.Either[Err,Failure]] = {
			Builder.this.finish(folding)
				.builderFailure.map(util.Right.apply _)
				.complex.flatMap{x => fun(x).fold(
					{x => ParserRetVal.BuilderFailure(util.Left(x))},
					ParserRetVal.Complex.apply _
				)}
		}
	}
	
	/**
	 * Create a Builder which builds two items from the same input. The first
	 * item built being the one that `this` would build, and the second item
	 * built being the one that `that` would build.
	 * 
	 * Most useful if the Parser input is neither stable nor resendable - say a network stream.
	 * 
	 * @param that the other builder
	 * @since 3.1
	 */
	final def zip[K2, V2, F2, R2](that:Builder[K2, V2, F2, R2])(implicit evk: K2 <:< Key, evv: V2 <:< Value, evf: Failure <:< F2):Builder[K2,V2,F2,(Result, R2)] = new Builder[K2,V2,F2,(Result,R2)] {
		private[this] val astBuilder = MapBuilder.apply[K2, V2]
		private[this] val ast2Parser = new com.rayrobdod.json.parser.RecursiveMapParser[K2, V2]
		private[this] val ident2Parser = new com.rayrobdod.json.parser.IdentityParser[V2]
		private[this] val ast1Parser = ast2Parser.mapKey(evk).mapValue(evv)
		private[this] val ident1Parser = ident2Parser.mapValue(evv)
		
		override type Middle = ((Builder.this.Middle, that.Middle))
		override def init:Middle = ((Builder.this.init, that.init))
		
		override def finish(folding:Middle):ParserRetVal[(Result, R2), Nothing, Nothing, F2] = {
			for (
				left <- Builder.this.finish(folding._1).builderFailure.map(evf).complex;
				right <- that.finish(folding._2).complex
			) yield {
				((left, right))
			}
		}
		
		override def apply[Input, PF](a:Middle, key:K2, b:Input, c:Parser[K2, V2, PF, Input]):ParserRetVal[Middle, Nothing, PF, F2] = {
			// `b` might be mutable, so `c.parse(…, c)` can only be called once;
			// `ast` or `value` however are not, so those may be parsed multiple times 
			c.parse(astBuilder, b).fold(
				{ast:Map[K2, Either[MapBuilder.RecursiveSubject[K2, V2], V2]] =>
					val e1:ParserRetVal[Builder.this.Middle, Nothing, PF, F2] = Builder.this.apply(a._1, key, ast, ast1Parser).builderFailure.map(evf)
					val e2:ParserRetVal[that.Middle, Nothing, PF, F2] = that.apply(a._2, key, ast, ast2Parser)
					for {r1 <- e1.complex; r2 <- e2.complex} yield {((r1, r2))}
				},
				{value:V2 => 
					val e1 = Builder.this.apply(a._1, key, value, ident1Parser).builderFailure.map(evf)
					val e2 = that.apply(a._2, key, value, ident2Parser)
					for {r1 <- e1.complex; r2 <- e2.complex} yield {((r1, r2))}
				},
				ParserRetVal.ParserFailure.apply _,
				ParserRetVal.BuilderFailure.apply _
			)
		}
	}
	
}
