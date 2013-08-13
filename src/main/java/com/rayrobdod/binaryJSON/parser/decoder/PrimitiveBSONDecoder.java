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
 * A method that turns non-object elements into java objects. Can be called by other decoders to take
 * care of these items.
 * @author Raymond Dodge
 * @version 2013 Aug 03
 * @version 2013 Aug 04 - implementing regex patterns
 */
public final class PrimitiveBSONDecoder// implements BSONDecoder<Object>
{
	private PrimitiveBSONDecoder() {}
	
	public static Object decoder(byte type, DataInput input) throws NullPointerException, IOException, ParseException, UnsupportedOperationException
	{
		switch (type) {
			case 0x0A : return null;
				
			case 0x01 : // double
				return new Double(longBitsToDouble( Long.reverseBytes( input.readLong() ) ));
			case 0x12 : // long
				return new Long(                    Long.reverseBytes( input.readLong() ) );
			case 0x09 : // timestamp
				return new java.util.Date(          Long.reverseBytes( input.readLong() ) );
			case 0x10 : // integer
				return new Integer(              Integer.reverseBytes( input.readInt() ) );
			case 0x08 : // boolean
				return input.readByte() != 0x00;
			case 0x05 : { // binary
				int length = Integer.reverseBytes( input.readInt() );
				byte subtype = input.readByte();
				byte[] retVal = new byte[length];
				
				input.readFully( retVal );
				
				if ((0x03 == subtype || 0x04 == subtype) && (128 == length)) {
					DataInput recurse = new java.io.DataInputStream(new java.io.ByteArrayInputStream(retVal));
					return new java.util.UUID(Long.reverseBytes(recurse.readLong()), Long.reverseBytes(recurse.readLong()));
				} else {
					return retVal;
				}
			}
			case 0x0E :   // symbol
			case 0x02 : { // string
				int length = Integer.reverseBytes( input.readInt() ) - 1;
				byte[] data = new byte[length];
				input.readFully( data );
				
				String retVal = new String(data, UTF_8);
				
				if (input.readByte() != 0x00) throw new ParseException("String not terminated by a 0x00", 0);
				return retVal;
			}
			case 0x0B : { // regex
				String regex;
				{
					java.io.ByteArrayOutputStream data = new java.io.ByteArrayOutputStream();
					
					byte val = input.readByte();
					while (val != 0x00) {
						data.write(val);
						val = input.readByte();
					}
					
					regex = new String( data.toByteArray(), UTF_8);
				}
				
				int flags = 0;
				{
					byte val = input.readByte();
					while (val != 0x00) {
						switch (val) {
							case 'i': flags = flags | Pattern.CASE_INSENSITIVE; break;
							case 'm': flags = flags | Pattern.MULTILINE; break;
							case 's': flags = flags | Pattern.DOTALL; break;
							case 'u': flags = flags | Pattern.UNICODE_CASE |
									Pattern.UNICODE_CHARACTER_CLASS; break;
								
							case 'x': // verbose
							case 'l': // local dependent
							default:
						}
						val = input.readByte();
					}
				}
				
				return Pattern.compile(regex, flags);
			}
			
		}
		
		throw new UnsupportedOperationException("This does not understand type: " + type);
	}
}
