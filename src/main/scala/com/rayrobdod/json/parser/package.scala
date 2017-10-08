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
import scala.collection.Iterator
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
}

package parser {
	
	/**
	 * A reader who takes charaters from the specified iterator
	 * @since 3.1
	 */
	private[parser] final class Iterator2Reader(iterator:Iterator[Char]) extends java.io.Reader {
		override def read():Int = if (!iterator.hasNext) {-1} else {iterator.next().toInt}
		override def close():Unit = {}
		override def read(buff:Array[Char], off:Int, len:Int):Int = {
			var idx = off
			val limit = off + len
			var c = this.read()
			while (idx < limit && c >= 0) {
				buff(idx) = c.toChar
				idx = idx + 1
				c = this.read()
			}
			idx - off
		}
	}
	
	/**
	 * A class that wraps a Reader and provides both a single-char buffer
	 * and a count of how many characters have been read
	 * @since 3.1
	 */
	final class CountingReader(back:java.io.Reader) {
		private[this] var _idx : Int = -1
		def index : Int = _idx
		private[this] var repeat : Boolean = false
		private[this] var repeatedChar : Char = '\u0000'
		
		def goBackOne():Unit = {
			if (repeat) { throw new IllegalStateException("Already gone back one") }
			_idx = _idx - 1
			repeat = true
		}
		def read():Char = {
			_idx = _idx + 1
			if (repeat) {
				repeat = false
				repeatedChar
			} else {
				val retVal = back.read()
				if (retVal < 0) {
					throw new java.util.NoSuchElementException
				} else {
					repeatedChar = retVal.toChar
					repeatedChar
				}
			}
		}
	}
	
	/**
	 * A 'parser' that echos the value provided in its parse method
	 * 
	 * Somewhat useful to be the 'recursed' parser in cases where the 'root' parser has already decoded a value.
	 * @version 3.0
	 */
	final class IdentityParser[V] extends Parser[Nothing,V,V] {
		/** Returns `v` wrapped in a [[com.rayrobdod.json.union.ParserRetVal.Primitive]] */
		def parse[A](b:Builder[Nothing,V,A], v:V):ParserRetVal.Primitive[V] = ParserRetVal.Primitive(v)
	}
	
	/**
	 * IdentityParser factory methods
	 * @since 3.1
	 */
	object IdentityParser {
		def apply[V] = new IdentityParser[V]
		/** Equivalent to `new IdentityParser[V].mapValue[PV]` */
		def apply[PV,V](implicit valueMapping:V => PV) = new IdentityParser[V].mapValue[PV]
	}
	
	/**
	 * A 'parser' that always returns a Failure
	 * @version 3.0
	 */
	private[json] final class FailureParser extends Parser[Nothing,Nothing,Any] {
		def parse[A](b:Builder[Nothing,Nothing,A], v:Any):ParserRetVal.Failure = ParserRetVal.Failure("FailureParser", 0)
	}
}
