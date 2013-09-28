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
package com.rayrobdod.binaryJSON.parser.decoders;

import com.rayrobdod.binaryJSON.parser.listeners.ToHashMap;
import com.rayrobdod.binaryJSON.parser.listeners.ToArrayList;
import com.rayrobdod.binaryJSON.parser.BSONParser;
import com.rayrobdod.binaryJSON.parser.BSONDecoder;
import java.io.DataInput;
import java.io.IOException;
import java.text.ParseException;
import java.util.regex.Pattern;
import static java.lang.Double.longBitsToDouble;
import static java.nio.charset.StandardCharsets.UTF_8;

/**
 * This takes a String that is properly JSONEncoded and turns it into a Java Collections object
 * @author Raymond Dodge
 * @version 2013 Aug 03
 * @version 2013 Aug 04 - implementing regex patterns
 * @version 2013 Aug 04 - outsourcing to PrimitiveBSONDecoder
 */
public final class ToJavaCollectionBSONDecoder implements BSONDecoder<Object>
{
	public Object decode(byte type, DataInput input) throws NullPointerException, IOException, ParseException, UnsupportedOperationException
	{
		switch (type) {
			case 0x03 : { // object
				/* single-pass parsing seems to work here.
				 * I don't trust it, but I'll allow it.
				 */
				ToHashMap l = new ToHashMap<Object>(this);
				BSONParser.parse(l, input);
				return l.getResult();
			}
			case 0x04 : { // array
				/* single-pass parsing seems to work here.
				 * I don't trust it, but I'll allow it.
				 */
				ToArrayList l = new ToArrayList<Object>(this);
				BSONParser.parse(l, input);
				return l.getResult();
			}
			default:
				return PrimitiveBSONDecoder.decoder(type,input);
		}
	}
}
