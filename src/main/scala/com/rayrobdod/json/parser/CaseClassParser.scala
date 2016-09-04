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
package com.rayrobdod.json.parser;

import scala.reflect.runtime.universe.{runtimeMirror, newTermName}
import com.rayrobdod.json.builder.Builder
import com.rayrobdod.json.union.ParserRetVal


/**
 * A parser that reports the name and value of each member of a case class
 * 
 * @version 3.0
 * @tparam Input the type of object to be parsed
 * @constructor
 * Creates a CaseClassParser instance.
 * @param clazz a java.lang.Class instance that represents the Input
 */
@deprecated("Terribly un type-safe. Either use a new Parser subclass or an instance of BuildableParser instead", "3.0")
final class CaseClassParser[Input <: Product](implicit clazz:Class[Input]) extends Parser[String, Any, Input] {
	
	/**
	 * Reports the values inside obj to the builder
	 * @param obj the object to extract values from
	 */
	def parse[Output](builder:Builder[String, Any, Output], obj:Input):ParserRetVal[Output,Nothing] = {
		val mirror = runtimeMirror( this.getClass.getClassLoader )
		val typ = mirror.classSymbol( clazz ).toType
		val copyMethod = typ.declaration(newTermName("copy")).asMethod
		val copyParams = copyMethod.paramss(0)
		
		val resultEither = copyParams.zipWithIndex.foldLeft[Either[(String,Int),Output]](Right(builder.init)){(state:Either[(String,Int),Output], keyValue) =>
			val (name, index) = keyValue
			val name2 = name.name.decodedName.toString
			val value = obj.productElement(index)
			
			state.right.flatMap{x => builder.apply(x, name2, value, new IdentityParser[Any])}
		}
		ParserRetVal.eitherToComplex(resultEither)
	}
}
