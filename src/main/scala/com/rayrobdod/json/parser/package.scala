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
import com.rayrobdod.json.union.ParserRetVal

/**
 * Contains the various built-in parsers
 *
 * Most built-in parsers either parse a serialized form (json, cbor, csv),
 * parse from a generic collection class (seq, map) or parse a class that
 * conforms to a stereotype (case class)
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
	
	/**
	 * A string interpolater that hex-decodes strings into a sequence of bytes.
	 * Any characters outside the hex character range are filtered out and ignored.
	 * @example {{{
	 * 	hexSeq"0001 11FF" == immutable.Seq[Byte](0,1,17,-1)
	 * }}}
	 * @version 2.0
	 */
	private[json] implicit class HexArrayStringConverter(val sc: StringContext) extends AnyVal {
		def hexSeq(args: Nothing*):ISeq[Byte] = {
			sc.parts.mkString("","","")
				.filter{x =>
					('A' <= x && x <= 'F') || ('a' <= x && x <= 'f') || ('0' <= x && x <= '9')
				}
				.grouped(2)
				.map{x => Integer.parseInt(x, 16)}
				.map{_.byteValue}
				.to[ISeq]
		}
		def hexArray(args: Nothing*):Array[Byte] = {
			hexSeq(args:_*).toArray
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
	 * A parser that reads each key-value pair from a Map
	 * @version 3.0
	 * 
	 * @tparam K the type of keys contained in the Map
	 * @tparam V the type of values contained in the Map
	 * @constructor
	 * Create a MapParser
	 */
	final class MapParser[K,V] extends Parser[K,V,Map[K,V]] {
		/**
		 * Decodes the input values to an object.
		 * @param vals the sequence containing values
		 * @return the parsed object
		 */
		def parse[A](topBuilder:Builder[K,V,A], vals:Map[K, V]):ParserRetVal[A,V] = {
			vals.foldLeft[Either[(String,Int),A]](Right(topBuilder.init)){(state:Either[(String,Int),A], keyValue:(K, V)) => 
				val (key, value) = keyValue;
				state.right.flatMap{x => topBuilder.apply(x, key, value, new IdentityParser[K, V])}
			}
		}.fold({case (s,i) => ParserRetVal.Failure(s,i)},{a => ParserRetVal.Complex(a)})
	}
	
	/**
	 * A parser that reads each Value and its index from a Seq
	 * @version 3.0
	 * 
	 * @tparam V the type of values contained in the Seq
	 * @constructor
	 * Create a SeqParser
	 */
	final class PrimitiveSeqParser[V] extends Parser[Int,V,Seq[V]] {
		/**
		 * Decodes the input values to an object.
		 * @param vals the sequence containing values
		 * @return the parsed object
		 */
		def parse[A](topBuilder:Builder[Int,V,A], vals:Seq[V]):ParserRetVal[A,V] = {
			vals.zipWithIndex.foldLeft[Either[(String,Int),A]](Right(topBuilder.init)){(state:Either[(String,Int),A], valueKey:(V, Int)) => 
				val (value, key) = valueKey;
				state.right.flatMap{x => topBuilder.apply(x, key, value, new IdentityParser[Int, V])}
			}
		}.fold({case (s,i) => ParserRetVal.Failure(s,i)},{a => ParserRetVal.Complex(a)})
	}
	
	/**
	 * A parser that reads and parses each Value and its index from a Seq
	 * @version 3.0
	 * 
	 * @tparam K the type of key used by recurse
	 * @tparam V the type of primitiveValue used by recurse
	 * @tparam Inner the type of values contained in the Seq
	 * @constructor
	 * Create a SeqParser
	 * @param recurse a parser for values contained in the sequence
	 * @param keyMapping a mapping from integer indexies to type K.
	 */
	final class SeqParser[+K,+V,-Inner](recurse:Parser[K,V,Inner])(implicit keyMapping:Function1[Int, K]) extends Parser[K,V,Seq[Inner]] {
		def parse[A](topBuilder:Builder[K,V,A], vals:Seq[Inner]):ParserRetVal[A,V] = {
			vals.zipWithIndex.foldLeft[Either[(String,Int),A]](Right(topBuilder.init)){(state:Either[(String,Int),A], valueKey:(Inner, Int)) => 
				val (value, key2) = valueKey
				val key = keyMapping(key2)
				state.right.flatMap{x => topBuilder.apply(x, key, value, recurse)}
			}
		}.fold({case (s,i) => ParserRetVal.Failure(s,i)},{a => ParserRetVal.Complex(a)})
	}
	
	/**
	 * A 'parser' that echos the value provided in its parse method
	 * @version 3.0
	 */
	private[json] final class IdentityParser[+K,V] extends Parser[K,V,V] {
		/** Returns `scala.util.Right(v)` */
		def parse[A](b:Builder[K,V,A], v:V):ParserRetVal.Primitive[V] = ParserRetVal.Primitive(v)
	}
	
	/**
	 * A 'parser' that always returns a Failure
	 * @version 3.0
	 */
	private[json] final class FailureParser[+K,+V,-I] extends Parser[K,V,I] {
		def parse[A](b:Builder[K,V,A], v:I):ParserRetVal.Failure = ParserRetVal.Failure("FailureParser", 0)
	}
}
