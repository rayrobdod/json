package com.rayrobdod.javaScriptObjectNotation.parser.listeners;

import com.rayrobdod.javaScriptObjectNotation.parser.JSONParseListener;
import com.rayrobdod.javaScriptObjectNotation.parser.JSONParser;
import java.text.ParseException;

/**
 * This will determine the size of a JSONArray or JSONObject
 * when used in a {@link JSONParser}.
 * <p>
 * This class performs a minimal amount of error checking.
 * <p>
 * It also ignores empty elements; so that both <code>[,,]</code> and <code>[]</code> are zero.
 * 
 * @author Raymond Dodge
 * @version Sep 9, 2010
 * @version Nov 8, 2010 - changed from extending {@link JSONParseAdapter}
			to implementing {@link JSONParseListener}
 * @version 15 Dec 2011 - moved from {@code net.verizon.rayrobdod.javaScriptObjectNotation} to {@code com.rayrobdod.javaScriptObjectNotation;}
 * @version 15 Jan 2011 - moved from {@code com.rayrobdod.javaScriptObjectNotation.parser}
		to {@code com.rayrobdod.javaScriptObjectNotation.parser.listeners}
 * @version 2013 Jun 23 - renamed from SizeJSONParseListener to GetSize
 * 
 */
public final class GetSize implements JSONParseListener
{
	private int currentCount;
	private boolean isParsing;
	private boolean elementIsEmpty;

	/**
	 * restarts the item count
	 * @throws IllegalStateException never
	 */
	public void started() throws IllegalStateException
	{
		if (isParsing) throw new IllegalStateException("Is already parsing");
		isParsing = true;
		currentCount = 0;
	}
	
	public void elemStarted(int commaIndex, char character)
			throws IllegalStateException
	{
		if (!isParsing) throw new IllegalStateException("Is not parsing");
		
		elementIsEmpty = true;
	}
	
	public void charRead(int index, char character)
	{
		elementIsEmpty = false;
	}
	
	/**
	 * increments the number of items counted so far
	 * @param commaIndex unused
	 * @param character unused
	 * @throws IllegalStateException never
	 * @throws ParseException never
	 */
	public void elemEnded(int commaIndex, char character)
			throws IllegalStateException, ParseException
	{
		if (!isParsing) throw new IllegalStateException("Is not parsing");
		if (!elementIsEmpty)
		{
			currentCount++;
		}
	}
	
	/**
	 * Stops parsing, and prevents any further parsing events from happening until
	 * {@link #started()} is called again. 
	 * @throws IllegalStateException if this cannot accept parse events 
	 * @throws ParseException never
	 */
	public void ended() throws IllegalStateException, ParseException
	{
		if (!isParsing) throw new IllegalStateException("Is not parsing");
		isParsing = false;
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
	 * does nothing
	 * @param index the index of the ended bracket
	 * @param character the char of the ended bracket
	 * @throws IllegalStateException if a character should not be read
	 * @throws ParseException if the element is illegal in some way
	 */
	public void endingBracket(int index, char character)
			throws IllegalStateException, ParseException {}
	
	/**
	 * does nothing
	 * @param colonIndex the index of the colon indicating the separation
	 * @param character the character of the colon
	 */
	public void keyValueSeparation(int colonIndex, char character) {}
	
	/**
	 * does nothing
	 * @param index the index of the opened bracket
	 * @param character the char that opened the bracket
	 * @throws IllegalStateException if a character should not be read
	 * @throws ParseException if the element is illegal in some way
	 */
	public void openingBracket(int index, char character)
			throws IllegalStateException, ParseException {}
	
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
