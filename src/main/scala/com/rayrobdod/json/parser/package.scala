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
package com.rayrobdod.json;

import com.rayrobdod.json.builder.Builder
import scala.collection.{Iterable, Iterator}
import scala.collection.immutable.{Seq => ISeq}
// import scala.collection.{AbstractIterable, AbstractIterator}
import scala.util.{Try, Success, Failure}

/**
 * Contains the various built-in parsers
 */
package object parser {
	/** @version 2.0 */
	private[json] def byteArray2DataInput(ba:Array[Byte]):java.io.DataInput = {
		new java.io.DataInputStream(
			new java.io.ByteArrayInputStream(
				ba
			)
		)
	}
	
	// String Interpolation
	/** @version 2.0 */
	private[json] implicit class HexArrayStringConverter(val sc: StringContext) extends AnyVal {
		def hexSeq(args: Any*):ISeq[Byte] = {
			((sc.parts.head):String)
				.filter{x =>
					('A' <= x && x <= 'F') || ('a' <= x && x <= 'f') || ('0' <= x && x <= '9')
				}
				.grouped(2)
				.map{x => Integer.parseInt(x, 16)}
				.map{_.byteValue}
				.to[ISeq]
		}
		def hexArray(args: Any*):Array[Byte] = {
			hexSeq(args).toArray
		}
	}
}

package parser {
	/**
	 * An iterable whose iterator reads characters from the reader one at a time
	 * @version 2.0
	 */
	private[parser] final class Reader2Iterable(r:java.io.Reader) extends Iterable[Char] {
		def iterator():Iterator[Char] = {
			new Iterator[Char]() {
				private[this] var nextChar:Int = r.read()
				override def next:Char = {
					val retVal = nextChar;
					nextChar = r.read();
					retVal.toChar
				}
				override def hasNext:Boolean = {
					nextChar != -1;
				}
			}
		}
	}
	
	/**
	 * A trivial "parser" that goes through the motions using each element of a map
	 * @version next
	 * 
	 * @constructor
	 * Create a MapParser
	 * @param topBuilder the builder that this parser will use when constructing objects
	 */
	final class MapParser[K,V] extends Parser[K,V,Map[K,V]] {
		/**
		 * Decodes the input values to an object.
		 * @param vals the sequence containing values
		 * @return the parsed object
		 */
		def parse[A](topBuilder:Builder[K,V,A], vals:Map[K, V]):Try[Left[A,V]] = {
			vals.foldLeft[Try[A]](Success(topBuilder.init)){(state:Try[A], keyValue:(K, V)) => 
				val (key, value) = keyValue;
				state.flatMap{x => topBuilder.apply(x, key, value, new IdentityParser)}
			}
		}.map{Left(_)}
	}
	
	/**
	 * A trivial "parser" that goes through the motions with each element of a seq
	 * @version next
	 * 
	 * @constructor
	 * Create a SeqParser
	 * @param topBuilder the builder that this parser will use when constructing objects
	 */
	final class SeqParser[V] extends Parser[Int,V,Seq[V]] {
		/**
		 * Decodes the input values to an object.
		 * @param vals the sequence containing values
		 * @return the parsed object
		 */
		def parse[A](topBuilder:Builder[Int,V,A], vals:Seq[V]):Try[Left[A,V]] = {
			vals.zipWithIndex.foldLeft[Try[A]](Success(topBuilder.init)){(state:Try[A], valueKey:(V, Int)) => 
				val (value, key) = valueKey;
				state.flatMap{x => topBuilder.apply(x, key, value, new IdentityParser)}
			}
		}.map{Left(_)}
	}
	
	/**
	 * A 'parser' that echos the value provided in its parse method
	 * @version next
	 */
	final class IdentityParser[K,V] extends Parser[K,V,V] {
		/** Returns `scala.util.Right(v)` */
		def parse[A](b:Builder[K,V,A], v:V):Success[Right[A,V]] = Success(Right(v))
	}
}
