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
 * This is an Adapter for JSONParseListener. All methods do nothing, except abort which
 * always returns false.
 * 
 * @author Raymond Dodge
 * @version Sep 9, 2010
 * @version 15 Dec 2011 - moved from {@code net.verizon.rayrobdod.javaScriptObjectNotation} to {@code com.rayrobdod.javaScriptObjectNotation;}
 */
public abstract class JSONParseAdapter implements JSONParseListener
{
	/**
	 * returns false
	 * @return false
	 * @throws IllegalStateException never 
	 */
	public boolean abort() throws IllegalStateException
	{
		return false;
	}
	
	/**
	 * does nothing
	 * @param index the index of the read char
	 * @param character the character read
	 * @throws IllegalStateException if a character should not be read
	 */
	public void charRead(int index, char character)
			throws IllegalStateException {}
	
	/**
	 * does nothing
	 * @param commaIndex the index of the ended element
	 * @param character the char that ended the element
	 * @throws IllegalStateException if a character should not be read
	 * @throws ParseException if the element is illegal in some way
	 */
	public void elemEnded(int commaIndex, char character)
			throws IllegalStateException, ParseException {}
	
	/**
	 * does nothing
	 * @param commaIndex the index of the ended element
	 * @param character the char that started the element
	 * @throws IllegalStateException if a character should not be read
	 */
	public void elemStarted(int commaIndex, char character)
			throws IllegalStateException {}
	
	/**
	 * does nothing
	 * @throws IllegalStateException if this should not end
	 * @throws ParseException if anything is amis
	 */
	public void ended() throws IllegalStateException, ParseException {}
	
	/**
	 * does nothing
	 * @param index the index of the ended bracket
	 * @param character the char of the ended bracket
	 * @throws IllegalStateException if a character should not be read
	 * @throws ParseException if the element is illegal in some way
	 */
	public void endingBracket(int index, char character)
			throws IllegalStateException, ParseException {}
	
	/**
	 * does nothing
	 * @param colonIndex the index of the colon indicating the separation
	 * @param character the character indicating the separation
	 * @throws IllegalStateException if this cannot accept such an event
	 * @throws ParseException if there is no way a key-value separation should occur here
	 * @throws ClassCastException if the key is not a JSONString, or cannot be otherwise parsed
	 */
	public void keyValueSeparation(int colonIndex, char character)
			throws IllegalStateException, ParseException, ClassCastException {}
	
	/**
	 * does nothing
	 * @param index the index of the opened bracket
	 * @param character the char that opened the bracket
	 * @throws IllegalStateException if a character should not be read
	 * @throws ParseException if the element is illegal in some way
	 */
	public void openingBracket(int index, char character)
			throws IllegalStateException, ParseException {}
	
	/**
	 * does nothing
	 * @throws IllegalStateException if this should not be starting
	 */
	public void started() throws IllegalStateException {}
	
}
