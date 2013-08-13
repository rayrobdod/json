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
 * @see HashMap
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
