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
