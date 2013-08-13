package com.rayrobdod.javaScriptObjectNotation.parser.decoders;

import java.text.ParseException;
import com.rayrobdod.javaScriptObjectNotation.JSONString;
import com.rayrobdod.javaScriptObjectNotation.parser.JSONParser;
import com.rayrobdod.javaScriptObjectNotation.parser.listeners.JSONObjectValidator;
import com.rayrobdod.javaScriptObjectNotation.parser.listeners.JSONArrayValidator;

/**
 * Methods that determine whether this is a valid item of the specified type
 * 
 * @author Raymond Dodge
 * @version 16 Jan 2012
 * @version 2013 Jun 23 - Now responds to IllegalStateExceptions as well as ParseExceptions
 * @version 2013 Jun 23 - extracting methods from package object; porting from Scala to Java
 */
public final class isValid {
	private isValid() {}
	
	/**
	 * Tests if a string is a valid array using {@link JSONArrayValidator}
	 * @param c the string to test
	 * @return true iff this string is a JSONEncoded string and an array
	 */
	public static boolean jsonArray(String c)
	{
		try {
			JSONArrayValidator l = new JSONArrayValidator();
			JSONParser.parse(l,c);
		} catch (ParseException e) {
			return false;
		} catch (IllegalStateException e) {
			return false;
		}
		return true;
	}
	
	/**
	 * Tests if a string is a valid object using {@link JSONObjectValidator}
	 * @param c the string to test
	 * @return true iff this string is a JSONEncoded string and an object
	 */
	public static boolean jsonObject(String c)
	{
		try {
			JSONObjectValidator l = new JSONObjectValidator();
			JSONParser.parse(l,c);
		} catch (ParseException e) {
			return false;
		} catch (IllegalStateException e) {
			return false;
		}
		return true;
	}
}
