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
	 * @param c the CharSequence to decode
	 * @return a Seq, JSONString, Number, Boolean or null
	 * @throws NullPointerException if c is null
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
