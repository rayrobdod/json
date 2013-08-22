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

package com.rayrobdod.javaScriptObjectNotation.parser;

import java.text.ParseException;

/**
 * Modified and extracted ParseListener from JSONArray
 * 
 * @author Raymond Dodge
 * @version Sep 1, 2010 - Sept 7, 2010
 * @version 15 Dec 2011 - moved from {@code net.verizon.rayrobdod.javaScriptObjectNotation} to {@code com.rayrobdod.javaScriptObjectNotation;}
 */
public interface JSONParseListener extends java.util.EventListener
{	
	/**
	 * A character has been read
	 * @param index the index of the character
	 * @param character the read character
	 * @throws IllegalStateException if this is called before started or between started and ended
	 */
	void charRead(int index, char character) throws IllegalStateException;
	
	/**
	 * An element has started
	 * @param commaIndex the index of the comma
	 * @param character the character that started the element
	 * @throws IllegalStateException if this is called before started or between started and ended
	 */
	void elemStarted(int commaIndex, char character) throws IllegalStateException;
	
	/**
	 * An element has ended
	 * @param commaIndex the index of the comma
	 * @param character the character that ended the element
	 * @throws IllegalStateException if this is called before started or between started and ended
	 * @throws ParseException if there was a problem with the element
	 */
	void elemEnded(int commaIndex, char character) throws IllegalStateException, ParseException;
	
	/**
	 * A key has ended and a value has started
	 * @param colonIndex the index of the comma
	 * @param character the character that split the element
	 * @throws IllegalStateException if this is called before started or between started and ended
	 * @throws ParseException if there was a problem with the key
	 * @throws ClassCastException if the key is not of the JSONString type, or a similar error occurs.
	 */
	void keyValueSeparation(int colonIndex, char character)
			throws IllegalStateException, ParseException, ClassCastException;

	/**
	 * Indicates that the initial bracket has been reached
	 * @param index the location of the bracket
	 * @param character the bracket character
	 * @throws IllegalStateException if this is called before started or between started and ended
	 * @throws ParseException if there bracket was the wrong one
	 */
	void openingBracket(int index, char character) throws IllegalStateException, ParseException;

	/**
	 * The level has changed
	 * @param index the location of the change
	 * @param character the character used in the change
	 * @throws IllegalStateException if this is called before started or between started and ended
	 * @throws ParseException if there bracket was the wrong one
	 */
	void endingBracket(int index, char character) throws IllegalStateException, ParseException;
	
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
