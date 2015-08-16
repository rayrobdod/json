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
package com.rayrobdod.json.builder;

import scala.reflect.runtime.universe.{runtimeMirror, newTermName}

/** A builder that builds a Case Class
 * 
 * @tparam A the type of object to build
 * @constructor
 * @param clazz the class of the objects to build
 * @param init the starting point of the builder
 * @param childBuilders a map used directly by childBuilder
 */
final class CaseClassBuilder[A <: Product](
		val init:A,
		childBuilders:Function1[String, Builder[_]] = Map.empty
)(		implicit clazz:Class[A]
) extends Builder[A] {
	
	/**
	 * Sets the `key` bean property in the `folding` object
	 * 
	 * @todo maybe check for other primitive numeric types - IE a `setVal(Short)` when handed a `Long` or visa versa
	 */
	def apply(folding:A, key:String, value:Any):A = {
		val mirror = runtimeMirror( this.getClass.getClassLoader )
		val typ = mirror.classSymbol( clazz ).toType
		val copyMethod = typ.declaration(newTermName("copy")).asMethod
		val copyParams = copyMethod.paramss(0)
		val indexOfModification = copyParams.zipWithIndex.find{_._1.name.decodedName.toString == key}.map{_._2}
		
		indexOfModification match {
			case None => throw new IllegalArgumentException(key + " is not a member of case class " + folding.toString)
			case Some(x:Int) => {
				val newArgs = folding.productIterator.toSeq.updated(x, value)
				
				val copyMirror = clazz.getMethods.filter{_.getName == "copy"}.head
				resultType.cast(copyMirror.invoke(folding, newArgs.map{_.asInstanceOf[Object]}:_*))
			}
		}
	}
	
	/**
	 * Applies the key to the constructor parameter `childBuilders`
	 */
	def childBuilder(key:String):Builder[_] = childBuilders(key)
	
	/** Returns the constructor parameter `clazz` */
	val resultType:Class[A] = clazz
}
