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

package com.rayrobdod.javaScriptObjectNotation;

import com.rayrobdod.javaScriptObjectNotation.parser.*;
import com.rayrobdod.javaScriptObjectNotation.parser.listeners.*;
import java.net.URLConnection;
import java.io.InputStream;
import java.io.Reader;
import java.io.InputStreamReader;
import java.io.IOException;
import java.text.ParseException;

/**
 * Reads a URLConnection, and turns it either into an ArrayList or a HashMap via JSON Parsing. 
 */
public final class JSONContentHandler extends java.net.ContentHandler
{
	private Reader makeReader(URLConnection urlc) throws IOException {
		final String encoding = urlc.getContentEncoding();
		Reader reader = new java.io.StringReader("");
		if (encoding != null) {
			reader = new InputStreamReader(urlc.getInputStream(), encoding);
		} else {
			reader = new InputStreamReader(urlc.getInputStream());
		}
		return reader;
	}
	
	public Object getContent(URLConnection urlc) throws IOException {
		Reader reader = this.makeReader(urlc);
		
		Object retVal;
		
		try {
			JSONArrayValidator validatorA = new JSONArrayValidator();
			ToArrayList<?> converterA = ToArrayList.apply();
			
			reader.close();
			reader = this.makeReader(urlc);
			JSONParser.parse(validatorA, reader);
			
			reader.close();
			reader = this.makeReader(urlc);
			JSONParser.parse(converterA, reader);
			
			retVal = converterA.getArrayList();
		} catch (ParseException e1) {
			try {
				JSONObjectValidator validatorO = new JSONObjectValidator();
				ToHashMap converterO = ToHashMap.apply();
				
				reader.close();
				reader = this.makeReader(urlc);
				JSONParser.parse(validatorO, reader);
				
				reader.close();
				reader = this.makeReader(urlc);
				JSONParser.parse(converterO, reader);
				
				retVal = converterO.getResult();
			} catch (ParseException e2) {
				retVal = null;
			}
		}
		
		reader.close();
		return retVal;
	}
	
	public Object getContent(URLConnection urlc, Class[] classes) throws IOException {
		
		for (Class c : classes) {
			if (c == java.util.ArrayList.class || c == java.util.List.class) {
				Reader reader = new java.io.StringReader("");
				try {
					reader = this.makeReader(urlc);
					ToArrayList<?> l = ToArrayList.apply();
					JSONParser.parse(l, reader);
					reader.close();
					return l.getArrayList();
				} catch (ParseException p) {
					reader.close();
				}
			}
			if (c == java.util.HashMap.class || c == java.util.Map.class) {
				Reader reader = new java.io.StringReader("");
				try {
					reader = this.makeReader(urlc);
					ToHashMap<?> l = ToHashMap.apply();
					JSONParser.parse(l, reader);
					reader.close();
					return l.getResult();
				} catch (ParseException p) {
					reader.close();
				}
			}
		}
		
		return null;
	}

}
