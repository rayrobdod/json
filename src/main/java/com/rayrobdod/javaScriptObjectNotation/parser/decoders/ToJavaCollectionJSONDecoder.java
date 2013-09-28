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

import com.rayrobdod.javaScriptObjectNotation.parser.decoders.isValid;
import com.rayrobdod.javaScriptObjectNotation.parser.JSONDecoder;
import com.rayrobdod.javaScriptObjectNotation.parser.JSONParser;
import com.rayrobdod.javaScriptObjectNotation.parser.listeners.ToArrayList;
import com.rayrobdod.javaScriptObjectNotation.parser.listeners.ToHashMap;
import com.rayrobdod.javaScriptObjectNotation.JSONString;
import java.text.ParseException;

/**
 * This takes a String that is properly JSONEncoded and turns it into a Java Collections object
 * @author Raymond Dodge
 * @version 16 Jan 2012
 * @version 2013 Jun 23 - making method not static, and now extends JSONDecoder
 * @version 2013 Jun 23 - now uses {@link ToHashMap} for JSONObjects, instead of JSONObject
 */
public final class ToJavaCollectionJSONDecoder implements JSONDecoder<Object>
{
	/**
	 * This returns a valid decoding of a CharSequence into a Java Object
	 * @param c the CharSequence to decode
	 * @return a HashMap, ArrayList, JSONString, Number, Boolean or null
	 * @throws NullPointerException if c is null
	 * @throws ClassCastException if the string is does not match any associate types
	 */
	public Object decode(String c) throws NullPointerException, ClassCastException
	{
		if (c == null) throw new NullPointerException();
		
		c = c.trim();
		
		if (c.equalsIgnoreCase("null")) return null;
		if (c.equalsIgnoreCase("true")) return true;
		if (c.equalsIgnoreCase("false")) return false;
		else if (JSONString.isValid(c))
		{
			try
			{
				return JSONString.generateUnparsed(c);
			}
			catch (ParseException e)
			{
				throw new AssertionError(e);
			}
		}
		else if (isValid.jsonArray(c))
		{
			try
			{
				ToArrayList l = new ToArrayList<Object>(this);
				JSONParser.parse(l, c);
				return l.getArrayList();
			}
			catch (ParseException e)
			{
				throw new AssertionError(e);
			}
		}
		else if (isValid.jsonObject(c))
		{
			try
			{
				ToHashMap l = new ToHashMap<Object>(this);
				JSONParser.parse(l, c);
				return l.getResult();
			}
			catch (ParseException e)
			{
				throw new AssertionError(e);
			}
		}
		else
		{
			try
			{
				return Long.decode(c.toString());
			}
			catch (NumberFormatException e)
			{
				try
				{
					return Double.parseDouble(c.toString());
				}
				catch (NumberFormatException e1)
				{
					throw new ClassCastException("the object does not fit any of" +
							" the supported types: " + c + ".");
				}
			}
		}
	}
}
