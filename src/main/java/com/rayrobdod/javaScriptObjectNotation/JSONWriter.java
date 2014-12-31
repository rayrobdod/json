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

import static java.nio.charset.StandardCharsets.UTF_8;
import java.util.Map;
import java.util.List;
import java.io.DataOutput;
import java.io.IOException;
import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.text.ParseException;

/**
 * Writes a data structure to a file in JSON format
 * 
 * @author Raymond Dodge
 * @version 2013 Aug 03
 * @deprecated use JSONEncoders instead
 * @see com.rayrobdod.binaryJSON.BSONWriter
 */
public final class JSONWriter
{
	private JSONWriter() {}
	
	public static String encode(Map<String, ?> data)
			throws NullPointerException, IOException
	{
		StringBuilder sb = new StringBuilder("{ ");
		
		for (Map.Entry<String, ?> entry : data.entrySet()) {
			sb.append(
				JSONString.generateParsed(
						entry.getKey().toString()).getUnparsedString()
			);
			
			sb.append(':');
			writeValue( sb, entry.getValue() );
			sb.append(',');
		}
		sb.setCharAt(sb.length() - 1, '}');
		
		return sb.toString();
	}
	
	public static String encode(List<?> data)
			throws NullPointerException, IOException
	{
		StringBuilder sb = new StringBuilder("[ ");
		
		for (Object value : data) {
			writeValue( sb, value );
			sb.append(',');
		}
		sb.setCharAt(sb.length() - 1, ']');
		
		return sb.toString();
	}
	
	private static void writeValue(StringBuilder output, Object o)
			throws IOException {
		//
		
		if (o == null) {
			output.append("null");
		}
		else if (o instanceof Boolean) { // boolean
			boolean b = ((Boolean) o).booleanValue();
			output.append((b?"true":"false"));
		}
		else if (o instanceof Number) {
			Number n = ((Number) o);
			if (n.doubleValue() == n.longValue()) {
				output.append( Long.toString(n.longValue()) );
			} else {
				output.append( Double.toString(n.doubleValue()) );
			}
		}
		else if (o instanceof String) {
			output.append(
				JSONString.generateParsed(o.toString()
					).getUnparsedString()
			);
		}
		else if (o instanceof List<?>) {
				List<?> m1 = ((List<?>) o);
				output.append( encode(m1) );
		}
		else if (o instanceof Map<?,?>) {
				Map<?,?> m1 = ((Map<?,?>) o);
				Map<String, Object> m2 = new java.util.HashMap<String,Object>();
				
				for (Map.Entry<?,?> e : m1.entrySet()) {
					m2.put( e.getKey().toString(), e.getValue() );
				}
				output.append( encode(m2) );
		} else {
			throw new UnsupportedOperationException(o.getClass().getName());
		}
	}
}
