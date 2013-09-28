/*
	Copyright (c) 2012-2013, Raymond Dodge
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
package com.rayrobdod.javaScriptObjectNotation.parser.listeners;

import com.rayrobdod.javaScriptObjectNotation.parser.{JSONParseListener, JSONDecoder};
import scala.collection.immutable.{Vector, Seq => ISeq, Map => IMap}
import scala.collection.mutable.StringBuilder
import com.rayrobdod.javaScriptObjectNotation.JSONString
import java.text.ParseException
import com.rayrobdod.javaScriptObjectNotation.parser.decoders.{
		ToScalaCollectionJSONDecoder}

/**
 * Parses a JSON entity into a Scala collection. It can be turned into a 
 * seq with resultSeq, and into a map with resultMap.
 * 
 * This does not care a lot about errors in the JSON source - it will create
 * a Pair and put that in an array if it happens to find a colon in a JSONArray
 * for example - but if there is nested arrays, the decoder may have a problem
 * with invalid JSONObjects - in fact the default one does.
 * 
 * @param decoder - the object used to turn unparsed JSON objects into an appropriate class. 
 * @author Raymond Dodge
 * @version 03 Aug 2011
 * @version 15 Dec 2011 - moved from {@code net.verizon.rayrobdod.javaScriptObjectNotation.parser} to {@code com.rayrobdod.javaScriptObjectNotation.parser}
 * @version 15 Jan 2011 - moved from {@code com.rayrobdod.javaScriptObjectNotation.parser}
		to {@code com.rayrobdod.javaScriptObjectNotation.parser.listeners}
 * @version 16 Jan 2011 - modifying to use a JSONDecoder, now that they exist.
 * @version 2013 Jun 21 - result and resultMap return immutable collections now. They did before,
 		but they say they will now.
 * @version 2013 Jun 23 - changing name from ToSeqJSONParseListener to ToScalaCollection
 * @version 2013 Jun 23 - removing zero-arg constructor; adding type parameter; adding method resultSeq
 */
final class ToScalaCollection[A <: Object](val decoder:JSONDecoder[A])
		extends JSONParseListener
{
	private var vector:Vector[Either[A, (String,A)]] = Vector.empty
	private var builder:StringBuilder = StringBuilder.newBuilder
	private var key:Option[String] = None
	
	override def abort = false
	override def charRead(index:Int, charact:Char) = builder += charact
	override def started() {vector = Vector.empty}
	override def ended() {}
	override def elemStarted(index:Int, charact:Char) {}
	override def openingBracket(index:Int, charact:Char) {}
	override def endingBracket(index:Int, charact:Char) {}
	
	override def elemEnded(index:Int, charact:Char) =
	{
		if (!builder.toString.trim.isEmpty)
		{
			try
			{
				val value = decoder.decode(builder.toString())
				
				vector = vector :+ (key match {
					case Some(x) => Right( ((x, value)) )
					case None => Left(value)
				})
			}
			catch
			{
				case e:ClassCastException => 
				{
					val e1 = new ParseException("Element could not be decoded: " + builder.toString.trim, index);
					e1.initCause(e);
					throw e1;
				}
			}
		}
		
		builder = StringBuilder.newBuilder
		key = None
	}
	
	override def keyValueSeparation(index:Int, charact:Char) =
	{
		key = Some(JSONString.generateUnparsed(builder.toString).toString)
		builder = StringBuilder.newBuilder
	}
	
	/** Returns the parsed result. May include Pairs. */
	def result:ISeq[Either[A, (String,A)]] = vector
	
	/**
	 * Returns the parsed result converted to a Seq.
	 * Removes pair's keys
	 */
	def resultSeq:ISeq[A] = result.map{_ match {
		case Left(x) => x
		case Right( ((_, x)) ) => x
	}}
	
	/**
	 * Returns the parsed result converted to a Map.
	 * Most non-pair values are destroyed in the transition. 
	 */
	def resultMap:IMap[String,A] = result.map{_ match {
		case Left( x ) => ("", x)
		case Right( x ) => x
	}}.toMap
}

/**
 * A factory for ToScalaCollections
 * @version 2013 Jun 23
 */
object ToScalaCollection {
	/** Returns a ToScalaCollection using a ToScalaCollectionJSONDecoder */
	def apply() = new ToScalaCollection(ToScalaCollectionJSONDecoder)
	def apply[A <: AnyRef](decoder:JSONDecoder[A]) = new ToScalaCollection(decoder)
}
