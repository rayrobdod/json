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

package com.rayrobdod.binaryJSON;

import com.rayrobdod.binaryJSON.parser.*;
import com.rayrobdod.binaryJSON.parser.listeners.*;
import java.net.URLConnection;
import java.io.InputStream;
import java.io.DataInput;
import java.io.DataInputStream;
import java.io.IOException;
import java.text.ParseException;

/**
 * Reads a URLConnection, and turns it either into an ArrayList or a HashMap via JSON Parsing. 
 * @since 1.0.1 
 */
public final class BSONContentHandler extends java.net.ContentHandler
{
	private DataInput makeDataInput(URLConnection urlc) throws IOException {
		
		return new DataInputStream(urlc.getInputStream());
	}
	
	/**
	 * returns the contents of a URLConnection as either java.util.List[_] or a java.util.Map[_,_]
	 */
	public Object getContent(URLConnection urlc) throws IOException {
		DataInput reader = this.makeDataInput(urlc);
		
		Object retVal;
		
		try {
			ToHashMap<?> converterO = ToHashMap.apply();
			BSONParser.parse(converterO, reader);
			
			retVal = converterO.getResult();
		} catch (ParseException e2) {
			retVal = null;
		}
		
		return retVal;
	}
	
	public Object getContent(URLConnection urlc, Class[] classes) throws IOException {
		
		for (Class<?> c : classes) {
			if (c.isAssignableFrom(java.util.HashMap.class)) {
				try {
					DataInput reader = this.makeDataInput(urlc);
					ToHashMap<?> l = ToHashMap.apply();
					BSONParser.parse(l, reader);
					return l.getResult();
				} catch (ParseException p) {
					
				}
			}
		}
		
		return null;
	}

}
