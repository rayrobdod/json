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
import com.rayrobdod.binaryJSON.parser.BSONDecoder;
import com.rayrobdod.binaryJSON.parser.decoders.SkipAllBSONDecoder;
import com.rayrobdod.binaryJSON.parser.decoders.ToJavaCollectionBSONDecoder;
import java.text.ParseException;
import java.util.ArrayList;

/**
 * This when run through the BSONParser will result in an {@link java.util.ArrayList}.
 * 
 * @author Raymond Dodge
 * @version 2013 Aug 03
 * @version 2013 Aug 04 - adding apply methods
 *
 * @see ArrayList
 * @see com.rayrobdod.javaScriptObjectNotation.parser.listeners.ToArrayList
 */
public final class ToArrayList<E> implements BSONParseListener
{
	private ArrayList<E> result;
	private final BSONDecoder<E> decoder;

	public ToArrayList(BSONDecoder<E> decoder) {
		this.decoder = decoder;
	}
	
	/**
	* Creates a ToHashMap using a {@link ToJavaCollectionBSONDecoder} decoder
	 */
	public static ToArrayList<Object> apply() {
		return new ToArrayList<Object>(new ToJavaCollectionBSONDecoder());
	}
	
	public static <E> ToArrayList<E> apply(BSONDecoder<E> decoder) {
		return new ToArrayList<E>(decoder);
	}
	
	/**
	 * restarts the item count
	 * @throws IllegalStateException if #started is called twice between invocation of #ended
	 */
	public void started() throws IllegalStateException
	{
		result = new ArrayList<E>();
	}
	
	/**
	 * Stops parsing, and prevents any further parsing events from happening until
	 * {@link #started()} is called again. 
	 * @throws IllegalStateException if this cannot accept parse events 
	 * @throws ParseException 
	 */
	public void ended() {}
	
	public void newKeyValue(byte a, String key, DataInput c) throws NullPointerException, IOException, ParseException, UnsupportedOperationException {
		E value = decoder.decode(a, c);
		result.add(value);
	}
	 
	/**
	 * returns the generated HashMap
	 * @return the generated HashMap
	 */
	public ArrayList<E> getResult()
	{
		return result;
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
