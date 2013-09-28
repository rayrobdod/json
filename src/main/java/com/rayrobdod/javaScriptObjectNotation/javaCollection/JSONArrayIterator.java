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
import java.util.ArrayList;
import java.util.ConcurrentModificationException;
import java.util.ListIterator;
import java.util.NoSuchElementException;
import com.rayrobdod.javaScriptObjectNotation.parser.listeners.JSONArrayValidator;
import com.rayrobdod.javaScriptObjectNotation.parser.JSONParseAdapter;
import com.rayrobdod.javaScriptObjectNotation.parser.JSONParseListener;
import com.rayrobdod.javaScriptObjectNotation.parser.JSONParser;
import com.rayrobdod.javaScriptObjectNotation.javaCollection.parseListeners.NextJSONArrayParseListener;
import com.rayrobdod.javaScriptObjectNotation.JSONString;

/**
 * An Iterator that iterators through JSONArrays.
 * 
 * @author Raymond Dodge
 * @version 2012 Jan 14  - Copied, but modified, from JSONArray
 * @version 2012 Jan 14  - removing support for #add, #remove and #set
 */
public final class JSONArrayIterator implements ListIterator<Object>
{
	private String unparsed;
	private ArrayList<Integer> commaIndexies;
	/** returns the element this is currently before */
	private int elementBefore;
	/** if less than 0, previous was called last
	 * if greater than 0, next was called last
	 * if equals 0, the last called was invalidated */
	private int lastCalled;
	
	public JSONArrayIterator(String unparsed, int nextIndex)
	{
		this.unparsed = unparsed;
		commaIndexies = new ArrayList<Integer>();
		commaIndexies.add(unparsed.indexOf('['));
		lastCalled = 0;
		elementBefore = 0;
		
		while (this.nextIndex() < nextIndex) {this.next();}
	}
	
	public JSONArrayIterator(String unparsed)
	{
		this(unparsed, 0);
	}
	
	public Object next() throws NoSuchElementException, ConcurrentModificationException
	{
//		if (this.expectedModCount != JSONArray.this.modCount) throw
//				new ConcurrentModificationException();
		if (!this.hasNext()) throw new NoSuchElementException(); 
		
		NextJSONArrayParseListener l = new NextJSONArrayParseListener();
		int start = commaIndexies.get(elementBefore);
		
		try
		{
			JSONParser.parse(l, unparsed, start);
		}
		catch (ParseException e)
		{
			throw new AssertionError(e);
		}
		
		elementBefore++;
		lastCalled = 1;
		
		if (elementBefore == commaIndexies.size())
		{
			commaIndexies.add(l.getEndCommaIndex());
		}
		else
		{
			commaIndexies.set(elementBefore, l.getEndCommaIndex());
		}
	
		// TODO: turn into a logger
//		System.out.println(commaIndexies);
		
		return l.getParsedElement();
	}
	
	public boolean hasNext()
	{
		return unparsed.charAt(commaIndexies.get(elementBefore)) != ']'
			&& !(unparsed.substring(commaIndexies.get(elementBefore) + 1,
					unparsed.lastIndexOf(']')).matches("[\\s]*"));
	}
	
	public int nextIndex()
	{
		return elementBefore;
	}
	
	public Object previous()
	{
//		if (this.expectedModCount != JSONArray.this.modCount) throw
//				new ConcurrentModificationException();
		if (!this.hasPrevious()) throw new NoSuchElementException(); 
		
		NextJSONArrayParseListener l = new NextJSONArrayParseListener();
		int start = commaIndexies.get(elementBefore - 1) - 1;
		
		try
		{
			JSONParser.parse(l, unparsed, start);
		}
		catch (ParseException e)
		{
			throw new AssertionError(e);
		}
		
		elementBefore--;
		lastCalled = -1;
		
		if (elementBefore == commaIndexies.size())
		{
			commaIndexies.add(l.getEndCommaIndex());
		}
		else
		{
			commaIndexies.set(elementBefore, l.getEndCommaIndex());
		}
		
		return l.getParsedElement();
	}
	
	public boolean hasPrevious()
	{
		return (previousIndex() >= 0);
//		return unparsed.charAt(startStringIndexes.get(elementBefore - 1)) != '['
//			&& !(unparsed.substring(unparsed.indexOf('[') + 1,
//					startStringIndexes.get(elementBefore - 1)).matches("[\\s]*"));
	}
	
	public int previousIndex()
	{
		return elementBefore - 1;
	}
	
	public void add(Object o) throws ClassCastException, ConcurrentModificationException,
			UnsupportedOperationException
	{
		throw new UnsupportedOperationException();
/*
		if (this.expectedModCount != JSONArray.this.modCount) throw
				new ConcurrentModificationException();
		
		String encodedString = "," + JSONObject.encode(o);
		int newStartIndex = startStringIndexes.get(elementBefore) + encodedString.length();
		
		unparsed = unparsed.substring(0, startStringIndexes.get(elementBefore))
				+ encodedString
				+ unparsed.substring(startStringIndexes.get(elementBefore));
		
		lastCalled = 0;
		startStringIndexes.add(newStartIndex);
		elementBefore++;
*/
	}
	
	public void remove() throws IllegalStateException, ConcurrentModificationException,
			UnsupportedOperationException
	{
		throw new UnsupportedOperationException();
/*
	{
		if (this.expectedModCount != JSONArray.this.modCount) throw
				new ConcurrentModificationException();
		if (lastCalled == 0) throw new IllegalStateException();

		if (lastCalled > 0) //next() called last
		{
			unparsed = unparsed.substring(0, startStringIndexes.get(elementBefore - 1))
					+ unparsed.substring(startStringIndexes.get(elementBefore));
			
			elementBefore--;
		}
		else //lastCalled < 0 //previous() called last
		{
			NextJSONArrayParseListener l = new NextJSONArrayParseListener();
			int start = startStringIndexes.get(elementBefore);
			
			try
			{
				JSONParser.parse(l, unparsed, start);
			}
			catch (ParseException e)
			{
				throw new AssertionError(e);
			}
			
			unparsed = unparsed.substring(0, startStringIndexes.get(elementBefore))
					+ unparsed.substring(l.getEndComma());
		}
*/	}

	
	public void set(Object o) throws ClassCastException, IllegalStateException,
			ConcurrentModificationException, UnsupportedOperationException
	{
		throw new UnsupportedOperationException();
/*
	{
		if (this.expectedModCount != JSONArray.this.modCount) throw
				new ConcurrentModificationException();
		if (lastCalled == 0) throw new IllegalStateException();
		
		if (lastCalled > 0) //next() called last
		{
			String encodedString = JSONObject.encode(o);
			int newStringIndex = startStringIndexes.get(elementBefore - 1)
					+ encodedString.length();
			
			unparsed = unparsed.substring(0, startStringIndexes.get(elementBefore - 1))
					+ ',' + encodedString
					+ unparsed.substring(startStringIndexes.get(elementBefore));
			
			startStringIndexes.set(elementBefore, newStringIndex);
		}
		else //lastCalled < 0 //previous() called last
		{
			NextJSONArrayParseListener l = new NextJSONArrayParseListener();
			int start = startStringIndexes.get(elementBefore);
			
			try
			{
				JSONParser.parse(l, unparsed, start);
			}
			catch (ParseException e)
			{
				throw new AssertionError(e);
			}
			
			unparsed = unparsed.substring(0, startStringIndexes.get(elementBefore))
					+ ',' + JSONObject.encode(o)
					+ unparsed.substring(l.getEndComma());
		}
*/	}

	
}
