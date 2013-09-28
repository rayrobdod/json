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
import java.util.Map;
import com.rayrobdod.javaScriptObjectNotation.JSONString;
import com.rayrobdod.javaScriptObjectNotation.parser.decoders.ToJavaObjectJSONDecoder;

/**
 * An entry in a JSONObject
 * 
 * @author Raymond Dodge
 * @version 15 Jan 2012 - extracting from JSONObject
 * @version 15 Jan 2012 - eliminating support for mutations
 * @version 2013 Jun 23 - using ToJavaObjectJSONDecoder.decode instad of JSONObject.decode
 */
public final class JSONObjectEntry implements Map.Entry<JSONString, Object>
{
	private final String unparsed;
	private final int startComma;
	private final int colonLocation;
	private int endComma;
	private boolean wasRemoved;
	
	public JSONObjectEntry(int startComma, int colonLocation,
			int endComma, String unparsed)
	{
		this.unparsed = unparsed;
		this.startComma = startComma;
		this.colonLocation = colonLocation;
		this.endComma = endComma;
		this.wasRemoved = false;
	}
	
	public JSONString getKey() throws IllegalStateException
	{
		if (wasRemoved)
		{
			throw new IllegalStateException("Entry removed");
		}
		
		try
		{	// plus one to disclude the actual startComma
			return JSONString.generateUnparsed(unparsed.subSequence(
					startComma + 1, colonLocation));
		}
		catch (ParseException e)
		{
			throw new AssertionError(e);
		}
	}

	public Object getValue() throws IllegalStateException
	{
		if (wasRemoved)
		{
			throw new IllegalStateException("Entry removed");
		}
		
		// plus one to disclude the actual startComma
		return ToJavaObjectJSONDecoder.instance.decode(unparsed.substring(colonLocation + 1,
				endComma));
	}
	
	public void remove() throws IllegalStateException
	{
		throw new UnsupportedOperationException();
/*		
		if (wasRemoved)
		{
			throw new IllegalStateException("Entry removed");
		}
		
		unparsed = unparsed.substring(0, startComma) + 
				unparsed.substring(endComma);
		wasRemoved = true;
*/	}

	public Object setValue(Object o) throws IllegalStateException
	{
		throw new UnsupportedOperationException();
		
/*		if (wasRemoved)
		{
			throw new IllegalStateException("Entry removed");
		}
		
		Object returnValue = this.getValue();
		
		String encodedString = JSONObject.encode(o);
		int lengthChange = Math.abs(endComma - (colonLocation + 1)
				- encodedString.length());
		
		// plus one to include colon
		unparsed = unparsed.subSequence(0, colonLocation + 1).toString()
				+ encodedString
				+ unparsed.subSequence(endComma, unparsed.length());
		
		endComma = endComma + lengthChange;
		
		return returnValue;
*/	}
	
	public boolean equals(Object o)
	{
		if (o == null) return false;
		if (! (o instanceof Map.Entry<?,?>)) return false;
		
		JSONObjectEntry other = (JSONObjectEntry) o;		
		
		return (this.getKey()==null ? other.getKey()==null :
				this.getKey().equals(other.getKey()))
		&& (this.getValue()==null ? other.getValue()==null :
				this.getValue().equals(other.getValue()));
	}
	
	public int hashCode()
	{
		return (this.getKey()==null ? 0 : this.getKey().hashCode()) ^
			(this.getValue()==null ? 0 : this.getValue().hashCode());
	}
	
	public String toString()
	{
		return unparsed.toString().substring(startComma + 1, endComma);
	}
	
	public int getStartCommaIndex() {return startComma;}
	
	public int getEndCommaIndex() {return endComma;}
	
	public int getColonIndex() {return colonLocation;}
}
