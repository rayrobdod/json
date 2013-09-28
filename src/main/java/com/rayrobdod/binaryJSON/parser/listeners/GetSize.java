/*
	Copyright (c) 2013, Raymond Dodge
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
package com.rayrobdod.binaryJSON.parser.listeners;

import java.io.DataInput;
import java.io.IOException;
import java.text.ParseException;
import com.rayrobdod.binaryJSON.parser.BSONParseListener;
import com.rayrobdod.binaryJSON.parser.BSONParser;
import com.rayrobdod.binaryJSON.parser.decoders.SkipAllBSONDecoder;
import java.text.ParseException;

/**
 * This will determine the size of a BSONArray or BSONObject
 * when used in a {@link JSONParser}.
 * <p>
 * This class performs a minimal amount of error checking.
 * 
 * @author Raymond Dodge
 * @version 2013 Aug 03 - modified from {@link com.rayrobdod.javaScriptObjectNotation.parser.listeners.GetSize}
 * 
 */
public final class GetSize implements BSONParseListener
{
	private int currentCount;
	private boolean isParsing;

	/**
	 * restarts the item count
	 * @throws IllegalStateException if #started is called twice between invocation of #ended
	 */
	public void started() throws IllegalStateException
	{
		if (isParsing) throw new IllegalStateException("Is already parsing");
		isParsing = true;
		currentCount = 0;
	}
	
	/**
	 * Stops parsing, and prevents any further parsing events from happening until
	 * {@link #started()} is called again. 
	 * @throws IllegalStateException if this cannot accept parse events 
	 * @throws ParseException 
	 */
	public void ended() throws IllegalStateException, ParseException
	{
		if (!isParsing) throw new IllegalStateException("Is not parsing");
		isParsing = false;
	}
	
	public void newKeyValue(byte a, String b, DataInput c) throws NullPointerException, IOException, ParseException, UnsupportedOperationException {
		if (!isParsing) throw new IllegalStateException("Is not parsing");
		new SkipAllBSONDecoder().decode(a, c);
		currentCount++;
	}
	 
	/**
	 * returns the current number of items counted
	 * @return the number of items counted
	 */
	public int getCount()
	{
		return currentCount;
	}
	
	/**
	 * returns false
	 * @return false
	 * @throws IllegalStateException never 
	 */
	public boolean abort() throws IllegalStateException
	{
		return false;
	}
}
