package com.rayrobdod.javaScriptObjectNotation.parser.listeners;

import com.rayrobdod.javaScriptObjectNotation.parser.JSONParseListener;
import com.rayrobdod.javaScriptObjectNotation.parser.decoders.ToJavaCollectionJSONDecoder;
import java.text.ParseException;
import java.util.regex.Pattern;

/**
 * This is a JSONParseListener that when run through the JSONParser will be a strict
 * validator for a JSONArray.
 * 
 * @author Raymond Dodge
 * @version Sep 7, 2010
 * @version Sep 9, 2010 - adjustment due to change in {@link JSONObject#decode(String)}
 * @version Oct 2, 2010 - fixed to say empty lists are valid 
 * @version 15 Dec 2011 - moved from {@code net.verizon.rayrobdod.javaScriptObjectNotation} to {@code com.rayrobdod.javaScriptObjectNotation;}
 * @version 15 Jan 2011 - moved from {@code com.rayrobdod.javaScriptObjectNotation.parser}
		to {@code com.rayrobdod.javaScriptObjectNotation.parser.listeners}
 * @todo don't create child objects.
 */
//TODO make more specific exception classes
public final class JSONArrayValidator implements JSONParseListener
{
	private StringBuilder currentElement;
	private boolean isParsing;
	private boolean reachedOpeningBracket;
	private boolean reachedEndingBracket;
	private int index;

	private static final Pattern beginChar = Pattern.compile("[\\[]");
	private static final Pattern endChar = Pattern.compile("[\\]]");
	private static final ToJavaCollectionJSONDecoder decoder = new ToJavaCollectionJSONDecoder();
	
	public boolean abort()
	{
		return false;
	}
	
	public void charRead(int index, char character) throws IllegalStateException
	{
		if (!isParsing) throw new IllegalStateException("This is not ready for parsing." +
				"Call .started() first.");
		if (currentElement == null) throw new IllegalStateException(
				"charRead called before elemStarted called");
		
		currentElement.append(character);
	}
	
	public void elemEnded(int commaIndex, char character)
			throws IllegalStateException, ParseException, ClassCastException
	{
		if (!isParsing) throw new IllegalStateException("This is not ready for parsing." +
				"Call .started() first.");
		if (currentElement == null) throw new IllegalStateException(
				"elemEnded called before elemStarted called");
//		if (currentElement.length() == 0) throw new ParseException(
//				"empty element", commaIndex);
		
		try
		{
			//empty test
			String element = currentElement.toString().trim();
			
			if (element.isEmpty())
			{
				if (endChar.matcher(character + "").matches() && index == 0)
				{
					// is OK; is an empty list with no elements, so empty element is allowed
				}
				else
				{
					throw new ParseException("empty element", commaIndex);
				}
			}
			else
			{
				decoder.decode(element);
			}
			
			currentElement = null;
		}
		catch (ClassCastException e)
		{
			throw new ParseException("Item contined invalid item", commaIndex);
		}
	}
	
	public void elemStarted(int commaIndex, char character) throws IllegalStateException
	{
		if (!isParsing) throw new IllegalStateException("This is not ready for parsing." +
				"Call .started() first.");
		if (currentElement != null) throw new IllegalStateException(
				"elemStarted called after elemStarted before elemEnded called");
		
		currentElement = new StringBuilder();
		index++;
	}
	
	/**
	 * throws an exception
	 * @param colonIndex the index of the color
	 * @param character the character used as a color
	 * @throws ParseException Always; JSONArrays should not have key-value pairs.
	 */
	public void keyValueSeparation(int colonIndex, char character) throws ParseException
	{
		throw new ParseException("Array contained a key-value pair", colonIndex);
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
				"Wrong initial char used. Expected ']': Was '" + character + "'.", index);
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
				"Wrong initial char used. Expected '[': Was '" + character + "'.", index);
		if (reachedOpeningBracket) throw new ParseException(
				"Found two items in same stream.", index);
		
		reachedOpeningBracket = true;
		this.index = -1;
	}
}