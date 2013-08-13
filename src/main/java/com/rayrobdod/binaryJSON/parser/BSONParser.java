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

package com.rayrobdod.binaryJSON.parser;

import static java.nio.charset.StandardCharsets.UTF_8;
import java.io.DataInput;
import java.io.IOException;
import java.text.ParseException;

/**
 * This is the class that parses DataInput and calls functions in BSONParseListeners based on what it finds.
 * 
 * @author Raymond Dodge
 * @version 2013 Aug 03
 * @see BSONParseListener
 * @see com.rayrobdod.javaScriptObjectNotation.parser.JSONParser
 */
public final class BSONParser
{
	private BSONParser() {}
	
	/**
	 * This starts by firing <var>l</var>'s {@link JSONParseListener#started() started()}
	 * method. It then reads the length <small>then discards the value</small>, then repeatedly
	 * reads key names and tells the listener to read values, until either it runs out of values
	 * or the listener tells the parser to stop. It then calls {@link JSONParseListener#ended() ended()}.
	 * <p>
	 * 
	 * 
	 * @param l the ParseListener that recieves events from this parser
	 * @param reader the reader that this will read chars from. It is assumed to be at the beginning of the stream
	 * @param skipChars the number of characters to skip in the reader before starting. This assumes any value after zero is after the initial '{' or '['.
	 * @throws NullPointerException if either l or reader is null
	 * @throws ParseException if the parsing fails
	 * @throws IOException if the reader could not be read from
	 */
	public static void parse(BSONParseListener l, DataInput input)
			throws NullPointerException, ParseException, IOException
	{
		l.started();
		
		/* int length = */ Integer.reverseBytes( input.readInt() );
		
		byte type = input.readByte();
		while (!l.abort() && type != 0x00) {
			String key = readCString(input);
			l.newKeyValue(type, key, input);
			
			type = input.readByte();
		}
		
		l.ended();
	}
	
	private static String readCString(DataInput input) throws IOException {
		java.io.ByteArrayOutputStream data = new java.io.ByteArrayOutputStream();
		
		byte val = input.readByte();
		while (val != 0x00) {
			data.write(val);
			val = input.readByte();
		}
		
		return new String( data.toByteArray(), UTF_8);
	}
}
