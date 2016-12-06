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
import scala.reflect.runtime.universe.{runtimeMirror, newTermName}

/** A builder that builds a Case Class
 * 
 * @version 3.0
 * @tparam A the type of object to build
 * @tparam Value the primitive values produced by the parser
 * @constructor
 * @param clazz the class of the objects to build
 * @param init the starting point of the builder
 * @param childBuilders a map used directly by childBuilder
 */
@deprecated("Terribly un type-safe. Either create a new Builder subclass or an instance of PiecewiseBuilder", "3.0")
final class CaseClassBuilder[Value, A <: Product](
		val init:A,
		childBuilders:Function1[String, Option[Builder[String, Value, _]]] = {s:String => None}
)(		implicit clazz:Class[A]
) extends Builder[String, Value, A] {
	
	/**
	 * Sets the `key` bean property in the `folding` object
	 * 
	 * @todo maybe check for other primitive numeric types - IE a `setVal(Short)` when handed a `Long` or visa versa
	 */
	override def apply[Input](folding:A, key:String, input:Input, parser:Parser[String, Value, Input]):Either[(String, Int), A] = {
		val mirror = runtimeMirror( this.getClass.getClassLoader )
		val typ = mirror.classSymbol( clazz ).toType
		val copyMethod = typ.declaration(newTermName("copy")).asMethod
		val copyParams = copyMethod.paramss(0)
		val indexOfModification = copyParams.zipWithIndex.find{_._1.name.decodedName.toString == key}.map{_._2}
		
		val builder = childBuilders(key).getOrElse(new ThrowBuilder())
		
		// unwrap union values
		val value = {
			parser.parse(builder, input).mergeToEither[Any].right.map{_ match {
					case com.rayrobdod.json.union.StringOrInt.Left(x) => x
					case com.rayrobdod.json.union.StringOrInt.Right(x) => x
					case x:com.rayrobdod.json.union.JsonValue => x match {
						case com.rayrobdod.json.union.JsonValue.JsonValueNull => null
						case com.rayrobdod.json.union.JsonValue.JsonValueString(x) => x
						case com.rayrobdod.json.union.JsonValue.JsonValueNumber(x) => x
						case com.rayrobdod.json.union.JsonValue.JsonValueBoolean(x) => x
					}
					case x => x
				}
			}
		}
		
		indexOfModification match {
			case None => Left(key + " is not a member of case class " + folding.toString, 0)
			case Some(x:Int) => value.right.flatMap{valueUnwrapped => {
				val copyMirror = clazz.getMethods.filter{_.getName == "copy"}.head
				
				val newArgs = folding.productIterator.toSeq.updated(x, (valueUnwrapped match {
					case y:scala.math.BigDecimal if copyMirror.getParameterTypes.apply(x) == classOf[Long] => y.longValue
					case y:scala.math.BigDecimal if copyMirror.getParameterTypes.apply(x) == classOf[java.lang.Long] => y.longValue
					case y => y
				}))
				
				try {
					Right(clazz.cast(copyMirror.invoke(folding, newArgs.map{_.asInstanceOf[Object]}:_*)))
				} catch {
					case ex:ClassCastException => Left(ex.getMessage(), 0)
					case ex:IllegalArgumentException => Left(ex.getMessage(), 0)
				}
			}}
		}
	}
}
