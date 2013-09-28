/*
	Copyright (c) 2012-2013, Raymond Dodge
	All rights reserved.
	
	Redistribution and use in source and binary forms, with or without
	modification, are permitted provided that the following conditions are met:
		* Redistributions of source code must retain the above copyright
		  notice, this list of conditions and the following disclaimer.
		* Redistributions in binary form must reproduce the above copyright
		  notice, this list of conditions and the following disclaimer in the
		  documentation and/or other materials provided with the distribution.
		* Neither the name "<PRODUCT NAME>" nor the names of its contributors
		  may be used to endorse or promote products derived from this software
		  without specific prior written permission.
	
	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
	ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
	WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
	DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
	DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
	(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
	LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
	ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
	(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
	SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
package com.rayrobdod.javaScriptObjectNotation.javaCollection;

import java.text.ParseException;
import java.util.Iterator;
import java.util.Map.Entry;
import java.util.NoSuchElementException;
import com.rayrobdod.javaScriptObjectNotation.javaCollection.parseListeners.NextJSONObjectParseListener;
import com.rayrobdod.javaScriptObjectNotation.parser.JSONParser;
import com.rayrobdod.javaScriptObjectNotation.JSONString;

/**
 * An Iterator that iterators through JSONObjects.
 * 
 * @author Raymond Dodge
 * @version 15 Jan 2012 - extracting from JSONObject
 * @version 03 Jul 2012 - rewriting hasNext from a one liner into something without
		a NullPointerException. An unavoidable NullPointerException if the method is used
 */
public final class JSONObjectIterator implements Iterator<Entry<JSONString, Object>>
{
	private JSONObjectEntry prevEntry;
	private final String unparsed;
	
	
	public JSONObjectIterator(String unparsed)
	{
		this.prevEntry = null;
		this.unparsed = unparsed;
		
		if (unparsed == null)
			throw new NullPointerException("Parameter unparsed was null");
	}
	
	public JSONObjectEntry next()
	{
		if (!this.hasNext()) throw new NoSuchElementException(); 
		
		//disassociating each JSONObjectEntry's endComma value; #toArray() is broken otherwise
		int startAt = (prevEntry == null ? unparsed.indexOf('{') : prevEntry.getEndCommaIndex());
		
		NextJSONObjectParseListener l = new NextJSONObjectParseListener();
		
		try
		{
			JSONParser.parse(l, unparsed, startAt);
		}
		catch (ParseException e)
		{
			throw new AssertionError(e);
		}
	
		prevEntry = l.getEntry(unparsed);
		return prevEntry;
	}
	
	public boolean hasNext()
	{
		int leftIndex = (prevEntry == null ? unparsed.indexOf('{') : prevEntry.getEndCommaIndex());
		int rightIndex = unparsed.lastIndexOf('}');
		
		boolean endCommaIsComma = unparsed.charAt(leftIndex) != '}';
		boolean whitespaceRemains = unparsed.substring(leftIndex, rightIndex).matches("[\\s]*");
		
		return endCommaIsComma && !whitespaceRemains;
	}
	
	public void remove() throws IllegalStateException
	{
		if (prevEntry == null) {
			throw new IllegalStateException("nothing to remove");
		} else {
			prevEntry.remove();
		}
	}
	
}
