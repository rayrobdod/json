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
package com.rayrobdod.javaScriptObjectNotation.parser.listeners;

import java.io.Writer;
import java.io.StringWriter;
import java.io.IOException;
import java.text.ParseException;
import com.rayrobdod.javaScriptObjectNotation.JSONString;
import com.rayrobdod.javaScriptObjectNotation.parser.JSONParseAdapter;
import com.rayrobdod.javaScriptObjectNotation.parser.JSONParser;

/**
 * <p>
 * A {@link com.rayrobdod.javaScriptObjectNotation.parser.JSONParseListener}
 * that will write a minified version of the parsed JSONObject or JSONArray
 * </p><p>
 * An instance of this class should not be reused.
 * </p>
 * @author Raymond Dodge
 * @version 2013 Jun 25
 * @todo play with JSONStrings (?)
 */
public final class Minifier extends JSONParseAdapter
{
	private final Writer resultWriter;
	private final StringBuilder elementBuilder;
	
	
	/**
	 * Creates a Minifier that will write results to the specified writer
	 * @param resultWriter the object to write results to
	 */
	public Minifier(Writer resultWriter) {
		this.resultWriter = resultWriter;
		this.elementBuilder = new StringBuilder();
	}
	
	/**
	 * Writes the current value to the writer.
	 */
	public void elemEnded(int index, char character)
				throws IllegalStateException, ParseException {
		try {
			String elem = elementBuilder.toString().trim();
			elementBuilder.setLength(0);
			
			if (isPrimitive(elem)) {
				resultWriter.write(elem);
			} else {
				StringWriter w = new StringWriter();
				Minifier childMinifier = new Minifier(w);
				
				JSONParser.parse(childMinifier, elem);
				
				resultWriter.write(w.toString());
			}
			
			resultWriter.write(character);
		} catch (IOException e) {
			throw new IllegalStateException("Could not write to given writer", e);
		}
	}
	
	/**
	 * Registers a character to recurse over, if the char is not ','
	 */
	public void elemStarted(int index, char character)
				throws IllegalStateException {
		try {
			if (character != ',')
				resultWriter.write(character);
		} catch (IOException e) {
			throw new IllegalStateException("Could not write to given writer", e);
		}
	}
	
	/**
	 * Registers a character to recurse over
	 */
	public void charRead(int index, char character) {
		elementBuilder.append(character);
	}
	
	/**
	 * <p>
	 * Writes the current key to the writer, as well as the character parameter.
	 * </p><p> 
	 * This tends to assume the key is not an array or object.
	 * </p>
	 */
	public void keyValueSeparation(int colonIndex, char character)
				throws IllegalStateException {
		try {
			resultWriter.write(elementBuilder.toString().trim());
			resultWriter.write(character);
			elementBuilder.setLength(0);
		} catch (IOException e) {
			throw new IllegalStateException("Could not write to given writer", e);
		}
	}
	
	
	
	
	
	/**
	 * Returns true if this should not recurse into the thing represented by str
	 */
	private boolean isPrimitive(String str) {
		if (str.equalsIgnoreCase("null")) return true;
		if (str.equalsIgnoreCase("true")) return true;
		if (str.equalsIgnoreCase("false")) return true;
		if (JSONString.isValid(str)) return true;
		try {
			Double.parseDouble(str);
			
			return true;
		} catch (NumberFormatException e1) {
			return false;
		}
	}
}
