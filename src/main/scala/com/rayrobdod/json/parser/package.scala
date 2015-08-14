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
package com.rayrobdod.json;

import com.rayrobdod.json.builder.Builder
import scala.collection.{Iterable, Iterator}
import scala.collection.immutable.{Seq => ISeq}
// import scala.collection.{AbstractIterable, AbstractIterator}

/**
 * Contains the various built-in parsers
 */
package object parser {
	private[json] def byteArray2DataInput(ba:Array[Byte]):java.io.DataInput = {
		new java.io.DataInputStream(
			new java.io.ByteArrayInputStream(
				ba
			)
		)
	}
	
	// String Interpolation
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
	/** An iterable whose iterator reads characters from the reader one at a time */
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
	
	/** A trivial "parser" that does the parse thing with a map */
	final class MapParser[A](topBuilder:Builder[A]) {
		def parse(vals:Map[_ <: Any, _ <: Any]):A = {
			vals.foldLeft[A](topBuilder.init){
				(state:A, keyValue:(Any, Any)) => topBuilder.apply(state, keyValue._1.toString, keyValue._2)
			}
		}
	}
	
	/** A trivial "parser" that does the parse thing with a seq */
	final class SeqParser[A](topBuilder:Builder[A]) {
		def parse(vals:Seq[_ <: Any]):A = {
			vals.zipWithIndex.foldLeft[A](topBuilder.init){
				(state:A, valueKey:(Any, Int)) => topBuilder.apply(state, valueKey._2.toString, valueKey._1)
			}
		}
	}
}
