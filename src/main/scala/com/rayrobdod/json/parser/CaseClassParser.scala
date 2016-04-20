/*
	Copyright (c) 2015, Raymond Dodge
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


/**
 * A parser that takes each value in a CaseClass and reports
 * 
 * @tparam A the type of object to build
 * @constructor
 * Creates a CaseClassParser instance.
 * @param topBuilder the builder that this parser will use when constructing objects
 */
final class CaseClassParser[Input <: Product](implicit clazz:Class[Input]) extends Parser[String, Any, Input] {
	
	/**
	 * Decodes the input values to an object.
	 * @param obj the object to extract values from
	 * @param clazz the class of obj
	 * @return the parsed object
	 */
	def parseComplex[Output](builder:Builder[String, Any, Output], obj:Input):Output = {
		val mirror = runtimeMirror( this.getClass.getClassLoader )
		val typ = mirror.classSymbol( clazz ).toType
		val copyMethod = typ.declaration(newTermName("copy")).asMethod
		val copyParams = copyMethod.paramss(0)
		
		copyParams.zipWithIndex.foldLeft(builder.init){(state:Output, keyValue) =>
			val (name, index) = keyValue
			val name2 = name.name.decodedName.toString
			val value = obj.productElement(index)
			
			builder.apply(name2).apply(state, value, new IdentityParser)
		}
	}
	
	def parsePrimitive(i:Input):Any = throw new UnsupportedOperationException
	def parseEither[Output](builder:Builder[String, Any, Output], obj:Input):Either[Output,Any] = {
		Left(this.parseComplex(builder, obj))
	}
}
