package com.rayrobdod.binaryJSON.parser.decoders;

import com.rayrobdod.binaryJSON.parser.listeners.ToScalaCollection;
import com.rayrobdod.binaryJSON.parser.BSONParser;
import com.rayrobdod.binaryJSON.parser.BSONDecoder;
import java.io.DataInput;
import java.io.IOException;
import java.text.ParseException;
import java.util.regex.Pattern;
import java.lang.Double.longBitsToDouble;
import java.nio.charset.StandardCharsets.UTF_8;

/**
 * This takes a String that is properly JSONEncoded and turns it into a Scala Collections object
 * @author Raymond Dodge
 * @version 2013 Aug 03
 */
object ToScalaCollectionBSONDecoder extends BSONDecoder[Object]
{
	def decode(t:Byte, input:DataInput):Object =
	{
		t match {
			case 0x03 => { // object
				/* single-pass parsing seems to work here.
				 * I don't trust it, but I'll allow it.
				 */
				val l = new ToScalaCollection[Object](this);
				BSONParser.parse(l, input);
				return l.resultMap;
			}
			case 0x04 => { // array
				/* single-pass parsing seems to work here.
				 * I don't trust it, but I'll allow it.
				 */
				val l = new ToScalaCollection[Object](this);
				BSONParser.parse(l, input);
				return l.resultSeq;
			}
			case _ =>
				return PrimitiveBSONDecoder.decoder(t,input);
		}
	}
}
