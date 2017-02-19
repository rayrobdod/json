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

import scala.util.Either
import com.rayrobdod.json.parser.Parser

/**
 * An object which takes a series of key-value pairs and combines them into an object using
 * a series of fold-left style method calls.
 * 
 * @version 3.0
 * @see [[com.rayrobdod.json.parser.Parser]]
 * @tparam Key the type of keys used by the Parser that this Builder will be used by
 * @tparam Value the type of primitive value types used by the Parser that this Builder will be used by
 * @tparam Subject the type of object built by this Builder
 */
trait Builder[-Key, -Value, Subject] {
	/**
	 * An 'empty' object. Can be thought of as either the value used at the
	 * start of a fold-left operation, or as an object filled with default values.
	 */
	def init:Subject
	
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
	 * @return 
	 *   - A [[scala.util.Right]] containing the built value, or
	 *   - A [[scala.util.Left]] indicating an error message and error index
	 */
	def apply[Input](folding:Subject, key:Key, input:Input, parser:Parser[Key, Value, Input]):Either[(String, Int), Subject]
	
	
	/**
	 * Change the type of key that this builder requires
	 * @param fun a conversion function from the new key to this's key
	 * @since 3.0
	 */
	final def mapKey[K2](implicit fun:Function1[K2,Key]):Builder[K2,Value,Subject] = new Builder[K2,Value,Subject] {
		override def init:Subject = Builder.this.init
		override def apply[Input](a:Subject, key:K2, b:Input, c:Parser[K2, Value, Input]):Either[(String, Int), Subject] = {
			Builder.this.apply(a, fun(key), b, c.mapKey(fun))
		}
	}
	
	/**
	 * Change the type of key that this builder requires, with the option of indicating an error condition
	 * @param fun a conversion function from the new key to this's key
	 * @since 3.1
	 */
	final def flatMapKey[K2](fun:Function1[K2,Either[(String,Int),Key]]):Builder[K2,Value,Subject] = new Builder[K2,Value,Subject] {
		override def init:Subject = Builder.this.init
		override def apply[Input](a:Subject, key:K2, b:Input, c:Parser[K2, Value, Input]):Either[(String, Int), Subject] = {
			fun(key).right.flatMap{k2 => Builder.this.apply(a, k2, b, c.flatMapKey(fun))}
		}
	}
	
	/**
	 * Change the type of value that this builder requires
	 * @param fun a conversion function from the new value to this's value
	 * @since 3.0
	 */
	final def mapValue[V2](implicit fun:Function1[V2,Value]):Builder[Key,V2,Subject] = new Builder[Key,V2,Subject] {
		override def init:Subject = Builder.this.init
		override def apply[Input](a:Subject, key:Key, b:Input, c:Parser[Key, V2, Input]):Either[(String, Int), Subject] = {
			Builder.this.apply(a, key, b, c.mapValue(fun))
		}
	}
	
	/**
	 * Change the type of value that this builder requires, with the option of indicating an error condition
	 * @since 3.0
	 */
	final def flatMapValue[V2](fun:Function1[V2,Either[(String,Int),Value]]):Builder[Key,V2,Subject] = new Builder[Key,V2,Subject] {
		override def init:Subject = Builder.this.init
		override def apply[Input](a:Subject, key:Key, b:Input, c:Parser[Key, V2, Input]):Either[(String, Int), Subject] = {
			Builder.this.apply(a, key, b, c.flatMapValue(fun))
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
	final def zip[K2, V2, S2](that:Builder[K2, V2, S2])(implicit evk: K2 <:< Key, evv: V2 <:< Value):Builder[K2,V2,(Subject, S2)] = new Builder[K2,V2,(Subject, S2)] {
		private[this] val astBuilder = MapBuilder.apply[K2, V2]
		private[this] val ast2Parser = new com.rayrobdod.json.parser.RecursiveMapParser[K2, V2]
		private[this] val ident2Parser = new com.rayrobdod.json.parser.IdentityParser[V2]
		private[this] val ast1Parser = ast2Parser.mapKey(evk).mapValue(evv)
		private[this] val ident1Parser = ident2Parser.mapValue(evv)
		
		override def init:(Subject, S2) = ((Builder.this.init, that.init))
		override def apply[Input](a:(Subject, S2), key:K2, b:Input, c:Parser[K2, V2, Input]):Either[(String, Int), (Subject, S2)] = {
			// `b` might be mutable, so `c.parse(…, c)` can only be called once;
			// `ast` or `value` however are not, so those may be parsed multiple times 
			c.parse(astBuilder, b).fold(
				{ast:Map[K2, Either[MapBuilder.RecursiveSubject[K2, V2], V2]] =>
					val e1 = Builder.this.apply(a._1, key, ast, ast1Parser)
					val e2 = that.apply(a._2, key, ast, ast2Parser)
					for {r1 <- e1.right; r2 <- e2.right} yield {((r1, r2))}
				},
				{value:V2 => 
					val e1 = Builder.this.apply(a._1, key, value, ident1Parser)
					val e2 = that.apply(a._2, key, value, ident2Parser)
					for {r1 <- e1.right; r2 <- e2.right} yield {((r1, r2))}
				},
				{(s,i) => Left(s,i)}
			)
		}
	}
	
}
