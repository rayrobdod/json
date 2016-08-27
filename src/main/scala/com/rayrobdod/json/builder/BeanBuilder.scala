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

/** A builder that builds a JavaBean
 * 
 * As with anything that works with java beans, this requires the class
 * to have a zero-argument constructor and will interact with methods
 * of the form `setX`.
 * 
 * @version 3.0
 * @tparam Value the parser's primitive values
 * @tparam A the type of object to build
 * @constructor
 * Creates a BeanBuilder
 * @param clazz the class of the objects to build
 * @param childBuilders a map of keys to builders of non-primitive bean values
 */
final class BeanBuilder[Value, A](
			clazz:Class[A],
			childBuilders:Function1[String, Option[Builder[String, Value, _]]] = Map.empty.lift
) extends Builder[String, Value, A] {
	/**
	 * Creates an instance of clazz by calling the class's No Argument constructor.
	 */
	def init:A = clazz.getConstructor().newInstance()
	
	/**
	 * Sets the `key` bean property in the `folding` object
	 * 
	 * note: Unlike most implementations of Builder, this will mutate `folding`
	 * @return the input parameter `folding`
	 * @todo maybe check for other primitive numeric types - IE a `setVal(Short)` when handed a `Long` or visa versa
	 */
	def apply[Input](folding:A, key:String, input:Input, parser:Parser[String, Value, Input]):Either[(String, Int), A] = {
		val builder = childBuilders(key).getOrElse(new ThrowBuilder())
		
		// unwrap union values
		parser.parse(builder, input).mergeToEither[Any].right.flatMap{a =>
			val value = {
				a match {
					case com.rayrobdod.json.union.StringOrInt.Left(x) => x
					case com.rayrobdod.json.union.StringOrInt.Right(x) => x
					case x:com.rayrobdod.json.union.JsonValue => com.rayrobdod.json.union.JsonValue.unwrap(x)
					case x => x
				}
			}
			
			try {
				val m = clazz.getMethods.filter{_.getName == ("set" + key.head.toUpper + key.tail)}.head
				val newValue = value match {
					case y:scala.math.BigDecimal if m.getParameterTypes.apply(0) == classOf[java.lang.Long] => y.longValue
					case y if m.getParameterTypes.apply(0) == y.getClass => y
					case y => throw new NoSuchMethodException(clazz.getName + "::set" + key.head.toUpper + key.tail + " with parameter " + y.getClass.getName)
				}
				m.invoke(folding, newValue.asInstanceOf[Object])
				// the above line should have mutated `folding`.
				Right(folding)
			} catch {
				case ex:NoSuchMethodException => Left(ex.getMessage(), 0)
			}
		}
	}
}
