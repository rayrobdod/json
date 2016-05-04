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

import scala.language.higherKinds
import scala.util.{Try, Success, Failure}
import com.rayrobdod.json.parser.Parser

/**
 * A class that creates an object from a sequence of 'fold'-style method calls
 * 
 * @version next
 * @see com.rayrobdod.json.parser.Parser
 * @tparam Key the type of keys used by the Parser that this Builder will be used by
 * @tparam Value the type of primitive value types used by the Parser that this Builder will be used by
 * @tparam Subject the type of object built by this Builder
 */
trait Builder[Key, Value, Subject] {
	/**
	 * The starting point of the folding process
	 */
	def init:Subject
	
	/**
	 * A folding function
	 * @param folding the object to be modified. Must be either the return value of [[init]] or the return value of [[apply]]
	 * @param key the key of a key-value pair
	 * @param parser a parser for turning input into a value
	 * @param input the input to a parser
	 */
	def apply[Input](key:Key, folding:Subject, input:Input, parser:Parser[Key, Value, Input]):Try[Subject]
	
	
	/** Change the type of key that this builder requires */
	final def mapKey[K2](implicit fun:Function1[K2,Key]):Builder[K2,Value,Subject] = new Builder[K2,Value,Subject] {
		override def init:Subject = Builder.this.init
		override def apply[Input](key:K2, a:Subject, b:Input, c:Parser[K2, Value, Input]):Try[Subject] = {
			final class ReverseParser(innerParser:Parser[K2,Value,Input]) extends Parser[Key,Value,Input] {
				override def parse[Output](builder:Builder[Key,Value,Output], i:Input):Try[Either[Output, Value]] = innerParser.parse(builder.mapKey[K2](fun), i)
			}
			
			Builder.this.apply(fun(key), a, b, new ReverseParser(c))
		}
	}
	
	/** Change the type of value that this builder requires */
	final def mapValue[V2](implicit fun:Function1[V2,Value]):Builder[Key,V2,Subject] = new Builder[Key,V2,Subject] {
		override def init:Subject = Builder.this.init
		override def apply[Input](key:Key, a:Subject, b:Input, c:Parser[Key, V2, Input]):Try[Subject] = {
			final class ReverseParser(innerParser:Parser[Key,V2,Input]) extends Parser[Key,Value,Input] {
				override def parse[Output](builder:Builder[Key,Value,Output], i:Input):Try[Either[Output, Value]] = {
					innerParser.parse[Output](builder.mapValue[V2](fun), i) match {
						case Success(Left(x)) => Success(Left(x))
						case Success(Right(x)) => Success(Right(fun(x)))
						case Failure(x) => Failure(x)
					}
				}
			}
			
			Builder.this.apply(key, a, b, new ReverseParser(c))
		}
	}
	
}
