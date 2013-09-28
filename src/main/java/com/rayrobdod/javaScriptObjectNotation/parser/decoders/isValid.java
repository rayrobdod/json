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
package com.rayrobdod.javaScriptObjectNotation.parser.decoders;

import java.text.ParseException;
import com.rayrobdod.javaScriptObjectNotation.JSONString;
import com.rayrobdod.javaScriptObjectNotation.parser.JSONParser;
import com.rayrobdod.javaScriptObjectNotation.parser.listeners.JSONObjectValidator;
import com.rayrobdod.javaScriptObjectNotation.parser.listeners.JSONArrayValidator;

/**
 * Methods that determine whether this is a valid item of the specified type
 * 
 * @author Raymond Dodge
 * @version 16 Jan 2012
 * @version 2013 Jun 23 - Now responds to IllegalStateExceptions as well as ParseExceptions
 * @version 2013 Jun 23 - extracting methods from package object; porting from Scala to Java
 */
public final class isValid {
	private isValid() {}
	
	/**
	 * Tests if a string is a valid array using {@link JSONArrayValidator}
	 * @param c the string to test
	 * @return true iff this string is a JSONEncoded string and an array
	 */
	public static boolean jsonArray(String c)
	{
		try {
			JSONArrayValidator l = new JSONArrayValidator();
			JSONParser.parse(l,c);
		} catch (ParseException e) {
			return false;
		} catch (IllegalStateException e) {
			return false;
		}
		return true;
	}
	
	/**
	 * Tests if a string is a valid object using {@link JSONObjectValidator}
	 * @param c the string to test
	 * @return true iff this string is a JSONEncoded string and an object
	 */
	public static boolean jsonObject(String c)
	{
		try {
			JSONObjectValidator l = new JSONObjectValidator();
			JSONParser.parse(l,c);
		} catch (ParseException e) {
			return false;
		} catch (IllegalStateException e) {
			return false;
		}
		return true;
	}
}
