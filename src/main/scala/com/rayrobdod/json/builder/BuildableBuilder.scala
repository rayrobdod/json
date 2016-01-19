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

import scala.collection.immutable.Map

/** Inspired by https://github.com/scopt/scopt/
 * 
 * @constructor
 * 
 * @param init 
 */
final class BuildableBuilder[A](
		val init:A,
		defaultKeyDef:BuildableBuilder.KeyDef[A] = BuildableBuilder.throwKeyDef[A],
		keyDefs:Map[String, BuildableBuilder.KeyDef[A]] = Map.empty[String, BuildableBuilder.KeyDef[A]]
		)(implicit val resultType:Class[A]
) extends Builder[A] {
	
	def addDef(key:String, apply:(A, Any) => A, childBuilder:Builder[_]):BuildableBuilder[A] = {
		new BuildableBuilder[A](init, defaultKeyDef, keyDefs + (key -> BuildableBuilder.KeyDef[A](apply, childBuilder)))
	}
	def ignoreUnknownKeys:BuildableBuilder[A] = {
		new BuildableBuilder[A](init, BuildableBuilder.ignoreKeyDef[A], keyDefs)
	}
	
	
	def apply(folding:A, key:String, value:Any):A = {
		keyDefs.getOrElse(key, defaultKeyDef).apply(folding, value)
	}
	def childBuilder(key:String):Builder[_ <: Any] = keyDefs.getOrElse(key, defaultKeyDef).childBuilder
}

object BuildableBuilder{
	final case class KeyDef[A](apply:(A, Any) => A, childBuilder:Builder[_])
	
	def ignoreKeyDef[A] = KeyDef[A]({(a,b) => a}, new SeqBuilder)
	def throwKeyDef[A] = KeyDef[A]({(a,b) => throw new IllegalArgumentException("Unknown key")}, new SeqBuilder)
}
