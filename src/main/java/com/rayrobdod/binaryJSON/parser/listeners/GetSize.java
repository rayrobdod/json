package com.rayrobdod.binaryJSON.parser.listeners;

import java.io.DataInput;
import java.io.IOException;
import java.text.ParseException;
import com.rayrobdod.binaryJSON.parser.BSONParseListener;
import com.rayrobdod.binaryJSON.parser.BSONParser;
import com.rayrobdod.binaryJSON.parser.decoders.SkipAllBSONDecoder;
import java.text.ParseException;

/**
 * This will determine the size of a BSONArray or BSONObject
 * when used in a {@link JSONParser}.
 * <p>
 * This class performs a minimal amount of error checking.
 * 
 * @author Raymond Dodge
 * @version 2013 Aug 03 - modified from {@link com.rayrobdod.javaScriptObjectNotation.parser.listeners.GetSize}
 * 
 */
public final class GetSize implements BSONParseListener
{
	private int currentCount;
	private boolean isParsing;

	/**
	 * restarts the item count
	 * @throws IllegalStateException if #started is called twice between invocation of #ended
	 */
	public void started() throws IllegalStateException
	{
		if (isParsing) throw new IllegalStateException("Is already parsing");
		isParsing = true;
		currentCount = 0;
	}
	
	/**
	 * Stops parsing, and prevents any further parsing events from happening until
	 * {@link #started()} is called again. 
	 * @throws IllegalStateException if this cannot accept parse events 
	 * @throws ParseException 
	 */
	public void ended() throws IllegalStateException, ParseException
	{
		if (!isParsing) throw new IllegalStateException("Is not parsing");
		isParsing = false;
	}
	
	public void newKeyValue(byte a, String b, DataInput c) throws NullPointerException, IOException, ParseException, UnsupportedOperationException {
		if (!isParsing) throw new IllegalStateException("Is not parsing");
		new SkipAllBSONDecoder().decode(a, c);
		currentCount++;
	}
	 
	/**
	 * returns the current number of items counted
	 * @return the number of items counted
	 */
	public int getCount()
	{
		return currentCount;
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
