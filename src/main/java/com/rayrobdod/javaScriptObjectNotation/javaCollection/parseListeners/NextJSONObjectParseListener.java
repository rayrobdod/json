package com.rayrobdod.javaScriptObjectNotation.javaCollection.parseListeners;

import java.text.ParseException;
import com.rayrobdod.javaScriptObjectNotation.parser.decoders.ToJavaObjectJSONDecoder;
import com.rayrobdod.javaScriptObjectNotation.parser.JSONParseListener;
import com.rayrobdod.javaScriptObjectNotation.javaCollection.JSONObjectEntry;

/**
 * @author Raymond Dodge
 * @version 2012 Jan 14  - Copied, but modified, from JSONArray 
 * @version 2012 Jan 15 - Initializes colon now, and builds key and value separatly
 * @version 2013 Jun 23 - using ToJavaObjectJSONDecoder.decode instad of JSONObject.decode
 */
public final class NextJSONObjectParseListener implements JSONParseListener
{
	private StringBuilder keyBuilder;
	private StringBuilder valueBuilder;
	private int startComma;
	private int endComma;
	private int colon;
	
	public NextJSONObjectParseListener()
	{
		keyBuilder = new StringBuilder();
		valueBuilder = new StringBuilder();
		startComma = -1;
		endComma = -1;
		colon = -1;
	}
	
	public boolean abort() throws IllegalStateException
	{
		return endComma >= 0;
	}
	
	public void charRead(int index, char character)
				throws IllegalStateException
	{
		if (colon >= 0) valueBuilder.append(character);
		if (startComma >= 0 && colon < 0) keyBuilder.append(character);
	}
	
	public void elemEnded(int commaIndex, char character)
			throws IllegalStateException, ParseException
	{
		if (startComma >= 0) endComma = commaIndex;
	}
	
	public void started() throws IllegalStateException
	{
		keyBuilder = new StringBuilder();
		valueBuilder = new StringBuilder();
		startComma = -1;
		endComma = -1;
		colon = -1;
	}
	
	public void elemStarted(int commaIndex, char character)
						throws IllegalStateException
	{
		if (startComma < 0) startComma = commaIndex;
	}
	
	public void keyValueSeparation(int colonIndex, char character)
			throws IllegalStateException, ParseException
	{
		if (startComma >= 0) colon = colonIndex;
	}
	
	public String getUnparsedKey() {return keyBuilder.toString();}
	
	public Object getParsedKey() {return ToJavaObjectJSONDecoder.instance.decode(getUnparsedKey());}
	
	public String getUnparsedValue() {return valueBuilder.toString();}
	
	public Object getParsedValue() {return ToJavaObjectJSONDecoder.instance.decode(getUnparsedValue());}
	
	public int getStartCommaIndex() {return startComma;}
	
	public int getEndCommaIndex() {return endComma;}
	
	public int getColonIndex() {return colon;}
	
	public JSONObjectEntry getEntry(String unparsed) {
		return new JSONObjectEntry(startComma, colon, endComma, unparsed);
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
