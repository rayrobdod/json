package com.rayrobdod.javaScriptObjectNotation.parser.decoders;

import com.rayrobdod.javaScriptObjectNotation.scalaCollection.JSONEither
import java.text.ParseException;
import com.rayrobdod.javaScriptObjectNotation.JSONString
import com.rayrobdod.javaScriptObjectNotation.parser.JSONParser
import com.rayrobdod.javaScriptObjectNotation.parser.JSONDecoder
import com.rayrobdod.javaScriptObjectNotation.parser.listeners.ToScalaCollection

/**
 * This takes a String that is properly JSONEncoded and turns it into a Scala Collections object
 * 
 * @author Raymond Dodge
 * @version 2013 Jun 21
 * @version 2013 Jun 23 - changing refrences from package object to isValid object
 */
object ToScalaEitherJSONDecoder extends JSONDecoder[JSONEither]
{
	/**
	 * This returns a valid decoding of a CharSequence into a Scala Object
	 * @param c the CharSequence to decode
	 * @return a Seq, JSONString, Number, Boolean or null
	 * @throws NullPointerException if c is null
	 * @throws ClassCastException if the string is does not match any associate types
	 */
	def decode(c1:String):JSONEither =
	{
		if (c1 == null) throw new NullPointerException();
		
		val c:String = c1.trim();
		
		if (c.equalsIgnoreCase("null")) return JSONEither.getNull
		else if (c.equalsIgnoreCase("true")) return JSONEither(true)
		else if (c.equalsIgnoreCase("false")) return JSONEither(false)
		else if (JSONString.isValid(c)) {
			try {
				return JSONEither(JSONString.generateUnparsed(c).toString);
			} catch {
				case e:ParseException => throw new AssertionError(e);
			}
		}
		else if (isValid.jsonArray(c)) {
			val l = new ToScalaCollection(this)
			JSONParser.parse(l, c)
			return JSONEither(l.resultSeq)
		}
		else if (isValid.jsonObject(c)) {
			val l = new ToScalaCollection(this)
			JSONParser.parse(l, c)
			return JSONEither(l.resultMap)
		}
		else {
			try {
				return JSONEither(c.toString().toDouble);
			} catch {
				case e1:NumberFormatException => {
					throw new ClassCastException("the string does not fit any of" +
							" the supported types: " + c);
				}
			}
		}
	}
}
