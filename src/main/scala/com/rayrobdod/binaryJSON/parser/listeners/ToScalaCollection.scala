/*
	Copyright (c) 2013, Raymond Dodge
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
package com.rayrobdod.binaryJSON.parser.listeners;

import java.io.DataInput;
import java.io.IOException;
import java.text.ParseException;
import com.rayrobdod.binaryJSON.parser.BSONParseListener;
import com.rayrobdod.binaryJSON.parser.BSONParser;
import com.rayrobdod.binaryJSON.parser.BSONDecoder;
import com.rayrobdod.binaryJSON.parser.decoders.SkipAllBSONDecoder;
import java.text.ParseException;
import java.util.HashMap;


import scala.collection.immutable.{Vector, Seq => ISeq, Map => IMap}
import scala.collection.mutable.StringBuilder
import java.text.ParseException
import com.rayrobdod.binaryJSON.parser.decoders.{
		ToScalaCollectionBSONDecoder}

/**
 * Parses a BSON entity into a Scala collection. It can be turned into a 
 * seq with resultSeq, and into a map with resultMap.
 * 
 * @author Raymond Dodge
 * @version 2013 Aug 04
 * 
 * @see com.rayrobdod.javaScriptObjectNotation.parser.listeners.ToHashMap
 */
final class ToScalaCollection[A <: Object](decoder:BSONDecoder[A]) extends BSONParseListener
{
	private var vector:Vector[(String,A)] = Vector.empty
	
	/**
	 * restarts the item count
	 * @throws IllegalStateException if #started is called twice between invocation of #ended
	 */
	override def started() =
	{
		vector = Vector.empty;
	}
	
	/**
	 * Stops parsing, and prevents any further parsing events from happening until
	 * {@link #started()} is called again. 
	 * @throws IllegalStateException if this cannot accept parse events 
	 * @throws ParseException 
	 */
	override def ended() {}
	
	override def newKeyValue(a:Byte, key:String, c:DataInput) = {
		val value = decoder.decode(a, c);
		vector = vector :+ ((key, value));
	}
	
	override def abort = false

	
	/** Returns the parsed result. May include Pairs. */
	def result:ISeq[(String,A)] = vector
	
	/**
	 * Returns the parsed result converted to a Seq.
	 * Removes pair's keys
	 */
	def resultSeq:ISeq[A] = result.map{_._2}
	
	/**
	 * Returns the parsed result converted to a Map.
	 * Most non-pair values are destroyed in the transition. 
	 */
	def resultMap:IMap[String,A] = result.toMap
	
	//
}

/**
 * A factory for ToScalaCollections
 * @version 2013 Jun 23
 */
object ToScalaCollection {
	/** Returns a ToScalaCollection using a ToScalaCollectionJSONDecoder */
	def apply() = new ToScalaCollection(ToScalaCollectionBSONDecoder)
	def apply[A <: AnyRef](decoder:BSONDecoder[A]) = new ToScalaCollection(decoder)
}
