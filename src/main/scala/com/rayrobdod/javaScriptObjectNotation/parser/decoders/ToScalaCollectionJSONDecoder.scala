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
import com.rayrobdod.javaScriptObjectNotation.JSONString
import com.rayrobdod.javaScriptObjectNotation.parser.JSONParser
import com.rayrobdod.javaScriptObjectNotation.parser.JSONDecoder
import com.rayrobdod.javaScriptObjectNotation.parser.listeners.ToScalaCollection

/**
 * This takes a String that is properly JSONEncoded and turns it into a Scala Collections object
 * 
 * @author Raymond Dodge
 * @version 16 Jan 2012
 * @version 2013 Jun 23 - changing refrences from package object to isValid object
 * @version 2013 Jun 23 - responding to changes in ToScalaCollection
 */
object ToScalaCollectionJSONDecoder extends JSONDecoder[Object]
{
	/**
	 * This returns a valid decoding of a CharSequence into a Scala Object
	 * @param c1 the CharSequence to decode
	 * @return a Seq, JSONString, Number, Boolean or null
	 * @throws NullPointerException if c1 is null
	 * @throws ClassCastException if the string is does not match any associate types
	 */
	def decode(c1:String):Object =
	{
		if (c1 == null) throw new NullPointerException();
		
		val c:String = c1.trim();
		
		if (c.equalsIgnoreCase("null")) return null
		// "true" is not an object. "java.lang.Boolean.TRUE" is.
		else if (c.equalsIgnoreCase("true")) return java.lang.Boolean.TRUE
		else if (c.equalsIgnoreCase("false")) return java.lang.Boolean.FALSE
		else if (JSONString.isValid(c))
		{
			try
			{
				return JSONString.generateUnparsed(c);
			}
			catch
			{
				case e:ParseException => throw new AssertionError(e);
			}
		}
		else if (isValid.jsonArray(c))
		{
			val l = new ToScalaCollection(this)
			JSONParser.parse(l, c)
			return l.resultSeq
		}
		else if (isValid.jsonObject(c))
		{
			val l = new ToScalaCollection(this)
			JSONParser.parse(l, c)
			return l.resultMap
		}
		else
		{
			try
			{
				return new java.lang.Long(c.toString());
			}
			catch
			{
				case e:NumberFormatException => {
					try
					{
						return new java.lang.Double(c.toString());
					}
					catch
					{
						case e1:NumberFormatException => {
							throw new ClassCastException("the string does not fit any of" +
								" the supported types: " + c);
						}
					}
				}
			}
		}
	}
}
