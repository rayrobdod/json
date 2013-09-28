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
