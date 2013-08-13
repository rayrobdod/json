package com.rayrobdod.javaScriptObjectNotation.parser.listeners;

import com.rayrobdod.javaScriptObjectNotation.parser.JSONParseListener;
import com.rayrobdod.javaScriptObjectNotation.parser.JSONDecoder;
import com.rayrobdod.javaScriptObjectNotation.parser.decoders.ToJavaCollectionJSONDecoder;
import java.text.ParseException;
import java.util.ArrayList;

/**
 * This when run through the JSONParser will result in an {@link ArrayList}.
 * This is preferable to <code>new ArrayList(new JSONArray(string))</code>
 * if a {@link java.io.Reader Reader} has to be used in the parser, for example,
 * due to a huge file that would cause an {@link OutOfMemoryError} if 
 * attempted to turned into a String. Or, just because the {@link JSONArray} is
 * just unwanted.
 * 
 * @author Raymond Dodge
 * @version Sep 9, 2010
 * @version Nov 8, 2010 - changed from extending {@link JSONParseAdapter}
			to implementing {@link JSONParseListener}
 * @version 15 Dec 2011 - moved from {@code net.verizon.rayrobdod.javaScriptObjectNotation} to {@code com.rayrobdod.javaScriptObjectNotation;}
 * @version 15 Jan 2011 - moved from {@code com.rayrobdod.javaScriptObjectNotation.parser}
		to {@code com.rayrobdod.javaScriptObjectNotation.parser.listeners}
 * @version 2013 Jun 23 - renaming from JSONToArrayListParseListener to ToArrayList
 * @version 2013 Jun 23 - adding #ToArrayList(JSONDecoder)
 * @version 2013 Jun 23 - now accepts empty arrays
 * @version 2013 Jun 25 - adding a type parameter, and static apply methods
 * @see ArrayList
 */
public final class ToArrayList<E> implements JSONParseListener
{
	private ArrayList<E> list;
	private StringBuilder builder;
	private final JSONDecoder<E> decoder;
	
	public ToArrayList(JSONDecoder<E> decoder) {
		this.decoder = decoder;
	}
	
	/**
	* Creates a ToHashMap using a {@link ToJavaCollectionJSONDecoder} decoder
	 */
	public static ToArrayList<Object> apply() {
		return new ToArrayList<Object>(new ToJavaCollectionJSONDecoder());
	}
	
	public static <E> ToArrayList<E> apply(JSONDecoder<E> decoder) {
		return new ToArrayList<E>(decoder);
	}
	
	/**
	 * returns false.
	 * @return false. Always.
	 */
	public boolean abort()
	{
		return false;
	}
	
	/**
	 * Reads the given character
	 * 
	 * @param index the index of he character that was read
	 * @param character the character to read
	 * @throws IllegalStateException if the parser is in an inappropriate place to red characters
	 * 
	 */
	public void charRead(int index, char character)
			throws IllegalStateException
	{
		if (list == null) throw new IllegalStateException("Has not started parsing");
		if (builder == null) throw new IllegalStateException("Is not in an element");
		
		builder.append(character);
	}
	
	public void elemStarted(int commaIndex, char character)
			throws IllegalStateException
	{
		if (list == null) throw new IllegalStateException("Has not started parsing");
		
		builder = new StringBuilder();
	}
	
	public void elemEnded(int commaIndex, char character)
			throws IllegalStateException, ParseException
	{
		if (list == null) throw new IllegalStateException("Has not started parsing");
		
		try
		{
			if (0 != builder.length()) {
				list.add(decoder.decode(builder.toString()));
			}
		}
		catch (ClassCastException e)
		{
			ParseException e1 = new ParseException("Element could not be decoded", commaIndex);
			e1.initCause(e);
			throw e1;
		}
		
		builder = null;
	}
	
	public void openingBracket(int index, char character)
			throws IllegalStateException, ParseException
	{
		if (list == null) throw new IllegalStateException("Has not started parsing");
		if (character != '[') throw new ParseException("Invalid array opening:" +
				" expected '['; was '" + character + "'", index);
	}
	
	public void endingBracket(int index, char character)
			throws IllegalStateException, ParseException
	{
		if (list == null) throw new IllegalStateException("Has not started parsing");
		if (character != ']') throw new ParseException("Invalid array ending:" +
				" expected ']'; was '" + character + "'", index);
	}
	
	public void keyValueSeparation(int colonIndex, char character)
			throws IllegalStateException, ParseException
	{
		throw new ParseException("Array cannot have key-value pair", colonIndex);
	}
	
	public void started() throws IllegalStateException
	{
		list = new ArrayList<E>();
	}
	
	public void ended() throws IllegalStateException, ParseException
	{
	}
	
	/**
	 * returns the generated ArrayList
	 * @return the generated ArrayList
	 */
	public ArrayList<E> getArrayList()
	{
		return list;
	}
}
