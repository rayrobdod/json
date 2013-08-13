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
