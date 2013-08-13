package com.rayrobdod.binaryJSON.parser.listeners;

import java.io.DataInput;
import java.io.IOException;
import java.text.ParseException;
import com.rayrobdod.binaryJSON.parser.BSONParseListener;
import com.rayrobdod.binaryJSON.parser.BSONParser;
import com.rayrobdod.binaryJSON.parser.BSONDecoder;
import com.rayrobdod.binaryJSON.parser.decoders.ToJavaCollectionBSONDecoder;
import java.text.ParseException;
import java.util.HashMap;

/**
 * This when run through the BSONParser will result in an {@link java.util.HashMap}.
 * @author Raymond Dodge
 * @version 2013 Aug 03
 * @version 2013 Aug 04 - adding apply methods
 * 
 * @see HashMap
 * @see com.rayrobdod.javaScriptObjectNotation.parser.listeners.ToHashMap
 */
public final class ToHashMap<E> implements BSONParseListener
{
	private HashMap<String, E> result;
	private final BSONDecoder<E> decoder;

	public ToHashMap(BSONDecoder<E> decoder) {
		this.decoder = decoder;
	}
	
	/**
	* Creates a ToHashMap using a {@link ToJavaCollectionBSONDecoder} decoder
	 */
	public static ToHashMap<Object> apply() {
		return new ToHashMap<Object>(new ToJavaCollectionBSONDecoder());
	}
	
	public static <E> ToHashMap<E> apply(BSONDecoder<E> decoder) {
		return new ToHashMap<E>(decoder);
	}
	
	/**
	 * restarts the item count
	 * @throws IllegalStateException if #started is called twice between invocation of #ended
	 */
	public void started() throws IllegalStateException
	{
		result = new HashMap<String,E>();
	}
	
	/**
	 * Stops parsing, and prevents any further parsing events from happening until
	 * {@link #started()} is called again. 
	 * @throws IllegalStateException if this cannot accept parse events 
	 * @throws ParseException 
	 */
	public void ended() {}
	
	public void newKeyValue(byte a, String key, DataInput c) throws NullPointerException, IOException, ParseException, UnsupportedOperationException {
		E value = decoder.decode(a, c);
		result.put(key, value);
	}
	 
	/**
	 * returns the generated HashMap
	 * @return the generated HashMap
	 */
	public HashMap<String, E> getResult()
	{
		return result;
	}
	
	/**
	 * returns false
	 * @return false
	 * @throws IllegalStateException never 
	 */
	public boolean abort() throws IllegalStateException
	{
		return false;
	}
}
