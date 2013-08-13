package com.rayrobdod.javaScriptObjectNotation.parser.decoders;

import com.rayrobdod.javaScriptObjectNotation.javaCollection.JSONObject;
import com.rayrobdod.javaScriptObjectNotation.javaCollection.JSONArray;
import com.rayrobdod.javaScriptObjectNotation.parser.JSONDecoder;
import com.rayrobdod.javaScriptObjectNotation.JSONString;
import java.text.ParseException;

/**
 * This takes a String that is properly JSONEncoded and turns it into a Java Object
 * @author Raymond Dodge
 * @version 16 Jan 2012 - extracted from JSONObject
 * @version 18 Jan 2012 - gave a method to make an object version 
 * @version 2013 Jun 23 - making method not static, and now extends JSONDecoder
 */
public final class ToJavaObjectJSONDecoder implements JSONDecoder<Object>
{
	public static ToJavaObjectJSONDecoder instance = new ToJavaObjectJSONDecoder(); 
	
	/**
	 * This returns a valid decoding of a CharSequence into a Java Object
	 * @param c the CharSequence to decode
	 * @return a JSONObject, JSONArray, JSONString, Number, Boolean or null
	 * @throws NullPointerException if c is null
	 * @throws ClassCastException if the string is does not match any associate types
	 */
	public Object decode(String c) throws NullPointerException, ClassCastException
	{
		if (c == null) throw new NullPointerException();
		
		c = c.trim();
		
		if (c.equalsIgnoreCase("null")) return null;
		if (c.equalsIgnoreCase("true")) return true;
		if (c.equalsIgnoreCase("false")) return false;
		else if (JSONString.isValid(c))
		{
			try
			{
				return JSONString.generateUnparsed(c);
			}
			catch (ParseException e)
			{
				throw new AssertionError(e);
			}
		}
		else if (JSONArray.isValid(c))
		{
			try
			{
				return new JSONArray(c);
			}
			catch (ParseException e)
			{
				throw new AssertionError(e);
			}
		}
		else if (JSONObject.isValid(c))
		{
			try
			{
				return new JSONObject(c);
			}
			catch (ParseException e)
			{
				throw new AssertionError(e);
			}
		}
		else
		{
			try
			{
				return Long.decode(c.toString());
			}
			catch (NumberFormatException e)
			{
				try
				{
					return Double.parseDouble(c.toString());
				}
				catch (NumberFormatException e1)
				{
					throw new ClassCastException("the object does not fit any of" +
							" the supported types: " + c + ".");
				}
			}
		}
	}
}
