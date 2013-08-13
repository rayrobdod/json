package com.rayrobdod.javaScriptObjectNotation.parser.listeners;

import com.rayrobdod.javaScriptObjectNotation.parser.JSONParseListener;
import com.rayrobdod.javaScriptObjectNotation.parser.JSONDecoder;
import com.rayrobdod.javaScriptObjectNotation.parser.decoders.ToJavaCollectionJSONDecoder;
import com.rayrobdod.javaScriptObjectNotation.JSONString;
import java.text.ParseException;
import java.util.HashMap;

/**
 * This when run through the JSONParser will result in an {@link HashMap}.
 * This is preferable to <code>new HashMap(new JSONObject(string))</code>
 * if a {@link java.io.Reader Reader} has to be used in the parser, for example,
 * due to a huge file that would cause an {@link OutOfMemoryError} if
 * attempted to turned into a String. Or, just because the {@link JSONObject} is
 * just unwanted.
 * 
 * @author Raymond Dodge
 * @version 2013 Jun 23
 * @see HashMap
 */
public final class ToHashMap<E> implements JSONParseListener
{
	private HashMap<String, E> result;
	private String currentKey;
	private StringBuilder builder;
	private final JSONDecoder<E> decoder;
	
	public ToHashMap(JSONDecoder<E> decoder) {
		this.decoder = decoder;
	}
	
	/**
	* Creates a ToHashMap using a {@link ToJavaCollectionJSONDecoder} decoder
	 */
	public static ToHashMap<Object> apply() {
		return new ToHashMap<Object>(new ToJavaCollectionJSONDecoder());
	}
	
	public static <E> ToHashMap<E> apply(JSONDecoder<E> decoder) {
		return new ToHashMap<E>(decoder);
	}
	
	/**
	 * returns false.
	 * @return false. Always.
	 */
	public boolean abort() {
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
		if (result == null) throw new IllegalStateException("Has not started parsing");
		if (builder == null) throw new IllegalStateException("Is not in an element");
		
		builder.append(character);
	}
	
	public void elemStarted(int commaIndex, char character)
			throws IllegalStateException
	{
		if (result == null) throw new IllegalStateException("Has not started parsing");
		
		currentKey = "";
		builder = new StringBuilder();
	}
	
	public void elemEnded(int commaIndex, char character)
			throws IllegalStateException, ParseException
	{
		if (result == null) throw new IllegalStateException("Has not started parsing");
		
		try
		{
			if (0 != builder.length()) {
				result.put(currentKey, decoder.decode(builder.toString()));
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
		if (result == null) throw new IllegalStateException("Has not started parsing");
		if (character != '{') throw new ParseException("Invalid array opening:" +
			" expected '{'; was '" + character + "'", index);
	}
	
	public void endingBracket(int index, char character)
			throws IllegalStateException, ParseException
	{
		if (result == null) throw new IllegalStateException("Has not started parsing");
		if (character != '}') throw new ParseException("Invalid array ending:" +
			" expected '}'; was '" + character + "'", index);
	}
	
	public void keyValueSeparation(int colonIndex, char character)
			throws IllegalStateException, ParseException
	{
		currentKey = JSONString.generateUnparsed(builder).toString();
		builder.setLength(0);
	}
	
	public void started() throws IllegalStateException {
		result = new HashMap<String,E>();
	}
	
	public void ended() {}
	
	/**
	 * returns the generated HashMap
	 * @return the generated HashMap
	 */
	public HashMap<String, E> getResult()
	{
		return result;
	}
}
