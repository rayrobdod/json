package com.rayrobdod.javaScriptObjectNotation.javaCollection.parseListeners;

import java.text.ParseException;
import com.rayrobdod.javaScriptObjectNotation.parser.decoders.ToJavaObjectJSONDecoder;
import com.rayrobdod.javaScriptObjectNotation.parser.JSONParseListener;

/**
 * @author Raymond Dodge
 * @version 2012 Jan 14  - Copied, but modified, from JSONArray 
 * @version 2013 Jun 23 - using ToJavaObjectJSONDecoder.decode instad of JSONObject.decode
 */
public final class NextJSONArrayParseListener implements JSONParseListener
{
	private StringBuilder builder;
	private int startComma;
	private int endComma;
	
	public NextJSONArrayParseListener()
	{
		builder = new StringBuilder();
		startComma = -1;
		endComma = -1;
	}
	
	public boolean abort() throws IllegalStateException
	{
		return endComma >= 0;
	}
	
	public void charRead(int index, char character)
				throws IllegalStateException
	{
		if (startComma >= 0) builder.append(character);
	}
	
	public void elemEnded(int commaIndex, char character)
			throws IllegalStateException, ParseException
	{
		if (startComma >= 0) endComma = commaIndex;
	}
	
	public void started() throws IllegalStateException
	{
		builder = new StringBuilder();
		startComma = -1;
		endComma = -1;
	}
	
	public void elemStarted(int commaIndex, char character)
						throws IllegalStateException
	{
		if (startComma < 0) startComma = commaIndex;
	}
	
	public void keyValueSeparation(int colonIndex, char character)
			throws IllegalStateException, ParseException
	{
		throw new ParseException("Array cannot have key-value pair", colonIndex);
	}
	
	public String getUnparsedElement()
	{
		return builder.toString();
	}
	
	public Object getParsedElement()
	{
		return ToJavaObjectJSONDecoder.instance.decode(getUnparsedElement());
	}
	
	public int getStartCommaIndex()
	{
		return startComma;
	}
	
	public int getEndCommaIndex()
	{
		return endComma;
	}
	
	
	/** does nothing 
	 */
	public void ended() throws IllegalStateException,
			ParseException
	{}
	
	/** does nothing 
	 * @param index no effect
	 * @param character no effect
	 */
	public void endingBracket(int index, char character) {}
	
	/** does nothing 
	 * @param index no effect
	 * @param character no effect
	 */
	public void openingBracket(int index, char character) {}
}
