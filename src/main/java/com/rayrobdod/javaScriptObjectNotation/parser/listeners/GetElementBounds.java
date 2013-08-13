package com.rayrobdod.javaScriptObjectNotation.parser.listeners;

import com.rayrobdod.javaScriptObjectNotation.parser.JSONParseListener;
import java.text.ParseException;

/**
 * This JSONParseListener obtains the bounds (the comma indexes) of an element
 * in a JSON item.
 * 
 * @author Raymond Dodge
 * @version Sep 9, 2010
 * @version Oct 14, 2010 - now implements JSONParseListener instead of extending adapter
 * @version 15 Dec 2011 - moved from {@code net.verizon.rayrobdod.javaScriptObjectNotation}
		to {@code com.rayrobdod.javaScriptObjectNotation}
 * @version 15 Jan 2011 - moved from {@code com.rayrobdod.javaScriptObjectNotation.parser}
		to {@code com.rayrobdod.javaScriptObjectNotation.parser.listeners}
 * @version 2013 Jun 23 - renamed from BoundsJSONParseListener to GetElementBounds
 */
public final class GetElementBounds implements JSONParseListener
{
	private final int getIndex;
	
	private int currentIndex;
	private int elementStartIndex;
	private int elementEndIndex;
	private int keyValueSplitIndex;
	
	/**
	 * Creates a BoundsJSONParseListener that will find the bounds of the spcified
	 * index in the list or object parsed.
	 * @param index the index this should look for
	 */
	public GetElementBounds(int index)
	{
		getIndex = index;
	}
	
	public boolean abort() throws IllegalStateException
	{
		return (currentIndex > getIndex);
	}
	
	public void elemStarted(int commaIndex, char character)
			throws IllegalStateException
	{
		currentIndex++;

		if (currentIndex == getIndex) {elementStartIndex = commaIndex;}
	}
	
	public void keyValueSeparation(int colonIndex, char character)
			throws IllegalStateException, ParseException, ClassCastException
	{
		if (currentIndex == getIndex) {keyValueSplitIndex = colonIndex;}
	}
	
	public void elemEnded(int commaIndex, char character)
			throws IllegalStateException, ParseException
	{
		if (currentIndex == getIndex) {elementEndIndex = commaIndex;}
	}
	
	public void started() throws IllegalStateException
	{
		currentIndex = -1; // so that the first index is 0
		
		elementStartIndex = -1;
		elementEndIndex = -1;
		keyValueSplitIndex = -1;
	}

	/**
	 * returns the element's start index
	 * @return the element's start index
	 		This is the index of the bracket or comma
	 */
	public final int getElementStartIndex()
	{
		return elementStartIndex;
	}

	/**
	 * returns the element's end index 
	 * @return the element's end index
	 		This is the index of the bracket or comma
	 */
	public final int getElementEndIndex()
	{
		return elementEndIndex;
	}

	/**
	 * returns the element's separation index
	 * @return the element's separation index
	 * 		returns -1 if there is no such keyValueSplit.
	 *		This is the index of the colon
	 */
	public final int getKeyValueSplitIndex()
	{
		return keyValueSplitIndex;
	}

	public void charRead(int index, char character) {}
	public void ended() {}
	public void endingBracket(int index, char character) {}
	public void openingBracket(int index, char character) {}
	
}
