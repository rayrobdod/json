package com.rayrobdod.javaScriptObjectNotation.parser.listeners;

import com.rayrobdod.javaScriptObjectNotation.parser.JSONParseListener;
import com.rayrobdod.javaScriptObjectNotation.parser.decoders.ToJavaCollectionJSONDecoder;
import java.text.ParseException;
import java.util.regex.Pattern;
import com.rayrobdod.javaScriptObjectNotation.JSONString;

/**
 * This is a JSONParseListener that when run through the JSONParser will be a strict
 * validator for a JSONObject.
 * 
 * @author Raymond Dodge
 * @version Sep 11, 2010
 * @version 15 Dec 2011 - moved from {@code net.verizon.rayrobdod.javaScriptObjectNotation} to {@code com.rayrobdod.javaScriptObjectNotation;}
 * @version 15 Jan 2011 - moved from {@code com.rayrobdod.javaScriptObjectNotation.parser}
		to {@code com.rayrobdod.javaScriptObjectNotation.parser.listeners}
 * @version 2013 Jun 23 - now depends on ToJavaCollectionJSONDecoder.decode instead of JSONObject.decode
 */
public final class JSONObjectValidator implements JSONParseListener
{
	private StringBuilder currentKey;
	private StringBuilder currentValue;
	private boolean isParsing;
	private boolean reachedOpeningBracket;
	private boolean reachedEndingBracket;

	private static final Pattern beginChar = Pattern.compile("[{]");
	private static final Pattern endChar = Pattern.compile("[}]");
	private static final ToJavaCollectionJSONDecoder decoder = new ToJavaCollectionJSONDecoder();

	public boolean abort()
	{
		return false;
	}
	
	public void charRead(int index, char character) throws IllegalStateException
	{
		if (!isParsing) throw new IllegalStateException("This is not ready for parsing." +
				"Call .started() first.");
		if (currentKey == null) throw new IllegalStateException(
				"charRead called before elemStarted called");
		
		if (currentValue == null)
		{
			currentKey.append(character);
		}
		else
		{
			currentValue.append(character);
		}
	}
	
	public void elemEnded(int commaIndex, char character)
			throws IllegalStateException, ParseException, ClassCastException
	{
		if (!isParsing) throw new IllegalStateException("This is not ready for parsing." +
				"Call .started() first.");
		if (currentKey == null) throw new IllegalStateException(
				"elemEnded called before elemStarted called");
		if (currentKey.length() == 0) throw new ParseException(
				"empty key", commaIndex);
		if (currentValue == null) throw new IllegalStateException(
				"elemEnded called before keyValueSeparation called");
		if (currentValue.length() == 0) throw new ParseException(
				"empty value", commaIndex);
		
		try
		{
			Object key = decoder.decode(currentKey.toString());
			decoder.decode(currentValue.toString());
			
			if (! (key instanceof JSONString))
			{
				ParseException e1 = new ParseException("key was not a JSONString", commaIndex);
				e1.initCause(new ClassCastException("key was not a JSONString"));
				throw e1;
			}

			currentKey = null;
			currentValue = null;
		}
		catch (ClassCastException e)
		{
			ParseException e1 = new ParseException("Object contined invalid item", commaIndex);
			e1.initCause(e);
			throw e1;
		}
	}
	
	public void elemStarted(int commaIndex, char character) throws IllegalStateException
	{
		if (!isParsing) throw new IllegalStateException("This is not ready for parsing." +
				"Call .started() first.");
		if (currentKey != null) throw new IllegalStateException(
				"elemStarted called after elemStarted before elemEnded called");
		
		currentKey = new StringBuilder();
	}
	
	/**
	 * throws an exception
	 * @param colonIndex the index of the color
	 * @param character the character used as a color
	 * @throws ParseException Always; JSONArrays should not have key-value pairs.
	 */
	public void keyValueSeparation(int colonIndex, char character) throws ParseException
	{
		if (!isParsing) throw new IllegalStateException("This is not ready for parsing." +
				"Call .started() first.");
		if (currentKey == null) throw new IllegalStateException(
				"keyValueSeparation called before elemStarted or after elemEnded called");
		if (currentValue != null) throw new IllegalStateException(
				"there were two key-value separations in the same element");
		
		currentValue = new StringBuilder();
	}

	public void started() throws IllegalStateException
	{
		if (isParsing) throw new IllegalStateException("This is already parsing");
		
		isParsing = true;
		reachedOpeningBracket = false;
		reachedEndingBracket = false;
	}
	
	public void ended() throws ParseException, IllegalStateException
	{
		if (!isParsing) throw new IllegalStateException("Is not parsing; cannot end");
		if (!reachedOpeningBracket) throw new ParseException(
				"Found no array in stream", 0);
		if (!reachedEndingBracket) throw new ParseException(
				"Found no ending bracket in stream.", 0);
		
		isParsing = false;
	}

	public void endingBracket(int index, char character)
			throws IllegalStateException, ParseException
	{
		if (!isParsing) throw new IllegalStateException("This is not ready for parsing." +
				"Call .started() first.");
		if (!endChar.matcher(character+"").matches()) throw new ParseException(
				"Wrong initial char used. Expected '}': Was '" + character + "'.", index);
		if (reachedEndingBracket) throw new ParseException(
				"Found two items in same stream.", index);
		if (!reachedOpeningBracket) throw new ParseException(
				"Ending bracket without opening one", index);
		
		reachedEndingBracket = true;
	}

	public void openingBracket(int index, char character)
			throws IllegalStateException, ParseException
	{
		if (!isParsing) throw new IllegalStateException("This is not ready for parsing." +
				"Call .started() first.");
		if (!beginChar.matcher(character+"").matches()) throw new ParseException(
				"Wrong initial char used. Expected '{': Was '" + character + "'.", index);
		if (reachedOpeningBracket) throw new ParseException(
				"Found two items in same stream.", index);
		
		reachedOpeningBracket = true;
	}
}
