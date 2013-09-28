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

import com.rayrobdod.binaryJSON.parser.BSONDecoder;
import java.io.DataInput;
import java.io.IOException;
import java.text.ParseException;
import static java.lang.Double.longBitsToDouble;
import static java.nio.charset.StandardCharsets.UTF_8;

/**
 * Skips all elements, ignoring them.
 * @author Raymond Dodge
 * @version 2013 Aug 03
 */
public final class SkipAllBSONDecoder implements BSONDecoder<Integer>
{
	public Integer decode(byte type, DataInput input) throws NullPointerException, IOException, ParseException, UnsupportedOperationException
	{
		int toSkip = 0;
		
		switch (type) {
			case 0x06: // undefined
			case 0x0A: // null
			case -1: // min
			case 0x7F: // max
				toSkip = 0;
				break;
			
			case 0x08: // boolean
				toSkip = 1;
				break;
			
			case 0x10:
				toSkip = 4;
				break;
			
			case 0x01: // integer
			case 0x09: //
			case 0x11:
			case 0x12:
				toSkip = 8;
				break;
				
			case 0x07:
				toSkip = 12;
				break;
			
			case 0x03: // array
			case 0x04: // object
				// the 'skip' value includes the four bytes containing the skip value!
				toSkip = Integer.reverseBytes( input.readInt() ) - 4;
				break;
				
			case 0x02: // string
			case 0x0D: // code
			case 0x0E: // symbol
				toSkip = Integer.reverseBytes( input.readInt() );
				break;
			
			case 0x05:
				toSkip = Integer.reverseBytes( input.readInt() ) + 1;
				break;
				
			default:
				throw new UnsupportedOperationException("This does not understand type: " + type);
				
			// others are hard
		}
		
		input.skipBytes(toSkip);
		
		// since I have to return something...
		return toSkip;
	}
}
