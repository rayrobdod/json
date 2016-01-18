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

import scala.collection.immutable.Seq;

/** A builder that creates seqs
 * 
 * @constructor
 * A builder that will create seqs, where complex types are built by `childBuilder.getOrElse(this)`
 * @param childBuilder the type of this seq's complex child elements. If it is Nothing, it will default to making more SeqBuilders
 */
final class SeqBuilder[Key](childBuilder:Option[Builder[Key, _ <: Any]]) extends Builder[Key, Seq[Any]] {
	/** A Builder that creates seqs, where every complex type child is also a seq */
	def this() = {this(None)}
	/** A Builder that creates seqs, where every complex type is of type `childBuilder` */
	def this(childBuilder:Builder[Key, _ <: Any]) = {this(Some(childBuilder))}
	
	val init:Seq[Nothing] = Nil
	def apply(folding:Seq[Any], key:Key, value:Any):Seq[Any] = {
		folding :+ value
	}
	def childBuilder(key:Key):Builder[Key, _ <: Any] = childBuilder.getOrElse(new SeqBuilder[Key])
	val resultType:Class[Seq[_]] = classOf[Seq[_]]
}
