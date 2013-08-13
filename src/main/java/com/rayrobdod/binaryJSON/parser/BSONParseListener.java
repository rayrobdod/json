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

package com.rayrobdod.binaryJSON.parser;

import java.io.DataInput;
import java.text.ParseException;
import java.io.IOException;

/**
 * Modified and extracted ParseListener from JSONArray
 * 
 * @author Raymond Dodge
 * @version 2013 Aug 03
 */
public interface BSONParseListener
{	
	/**
	 * A character has been read
	 * @param typeOfValue 
	 * @param key the pair's key
	 * @param reader the input values, to obtain a value from
	 * @throws IllegalStateException if this is called before started or between started and ended
	 */
	void newKeyValue(byte typeOfValue, String key, DataInput reader) throws NullPointerException, IOException, ParseException, UnsupportedOperationException;
	
	
	/**
	 * returns whether the parsing can end at this time
	 * @return true if the parsing can end
	 * @throws IllegalStateException if this is called before started or between started and ended
	 */
	boolean abort() throws IllegalStateException;
	
	/**
	 * Called when the parsing ends.
	 * @throws IllegalStateException if this is called before started or between started and ended
	 * @throws ParseException if there was an error with the parsing
	 */
	void ended() throws IllegalStateException, ParseException;
	
	/**
	 * Called when the parsing starts.
	 * @throws IllegalStateException if this is called before started or between started and ended
	 */
	void started() throws IllegalStateException;
}
