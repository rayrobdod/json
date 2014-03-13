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
 * An interface that listens for ParseEvents. ParseEvents are created by the
 * JSONParser.parse method. So, basically, an instance of this recieves events
 * from JSONParser.Parse and the listener can act based on the events.
 * 
 * @author Raymond Dodge
 */
public interface JSONParseListener extends java.util.EventListener
{	
	/**
	 * A character has been read
	 * @param index the index of the character
	 * @param character the read character
	 * @throws IllegalStateException if this is called before started or ???
	 */
	void charRead(int index, char character) throws IllegalStateException;
	
	/**
	 * An element has started
	 * @param commaIndex the index of the comma
	 * @param character the character that started the element
	 * @throws IllegalStateException if this is called before started or ???
	 */
	void elemStarted(int commaIndex, char character) throws IllegalStateException;
	
	/**
	 * An element has ended
	 * @param commaIndex the index of the comma
	 * @param character the character that ended the element
	 * @throws IllegalStateException if this is called before started or ???
	 * @throws ParseException if there was a problem with the element
	 */
	void elemEnded(int commaIndex, char character) throws IllegalStateException, ParseException;
	
	/**
	 * A key has ended and a value has started
	 * @param colonIndex the index of the comma
	 * @param character the character that split the element
	 * @throws IllegalStateException if this is called before started or ???
	 * @throws ParseException if there was a problem with the key
	 * @throws ClassCastException if the key is not of the JSONString type, or a similar error occurs.
	 */
	void keyValueSeparation(int colonIndex, char character)
			throws IllegalStateException, ParseException, ClassCastException;

	/**
	 * Indicates that the initial bracket has been reached
	 * @param index the location of the bracket
	 * @param character the bracket character
	 * @throws IllegalStateException ???
	 * @throws ParseException if the bracket was the wrong one
	 */
	void openingBracket(int index, char character) throws IllegalStateException, ParseException;

	/**
	 * The ending bracket
	 * @param index the location of the change
	 * @param character the character used in the change
	 * @throws IllegalStateException ???
	 * @throws ParseException if the bracket was the wrong one
	 */
	void endingBracket(int index, char character) throws IllegalStateException, ParseException;
	
	/**
	 * returns a value indicating whether the parser can short-circut parsing.
	 * @return true if this listener has all the information it needs to create a result
	 * @throws IllegalStateException ???
	 */
	boolean abort() throws IllegalStateException;
	
	/**
	 * Called when the parsing ends.
	 * @throws IllegalStateException if this is called before started or twice without interviening start calls
	 * @throws ParseException if whatever post-processing results in an illegal result
	 */
	void ended() throws IllegalStateException, ParseException;
	
	/**
	 * Called when the parsing starts.
	 * @throws IllegalStateException if this is called twice without interviening ended calls
	 */
	void started() throws IllegalStateException;
}
