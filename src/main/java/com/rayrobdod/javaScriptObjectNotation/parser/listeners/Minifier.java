package com.rayrobdod.javaScriptObjectNotation.parser.listeners;

import java.io.Writer;
import java.io.StringWriter;
import java.io.IOException;
import java.text.ParseException;
import com.rayrobdod.javaScriptObjectNotation.JSONString;
import com.rayrobdod.javaScriptObjectNotation.parser.JSONParseAdapter;
import com.rayrobdod.javaScriptObjectNotation.parser.JSONParser;

/**
 * <p>
 * A {@link com.rayrobdod.javaScriptObjectNotation.parser.JSONParseListener}
 * that will write a minified version of the parsed JSONObject or JSONArray
 * </p><p>
 * An instance of this class should not be reused.
 * </p>
 * @author Raymond Dodge
 * @version 2013 Jun 25
 * @todo play with JSONStrings (?)
 */
public final class Minifier extends JSONParseAdapter
{
	private final Writer resultWriter;
	private final StringBuilder elementBuilder;
	
	
	/**
	 * Creates a Minifier that will write results to the specified writer
	 * @param resultWriter the object to write results to
	 */
	public Minifier(Writer resultWriter) {
		this.resultWriter = resultWriter;
		this.elementBuilder = new StringBuilder();
	}
	
	/**
	 * Writes the current value to the writer.
	 */
	public void elemEnded(int index, char character)
				throws IllegalStateException, ParseException {
		try {
			String elem = elementBuilder.toString().trim();
			elementBuilder.setLength(0);
			
			if (isPrimitive(elem)) {
				resultWriter.write(elem);
			} else {
				StringWriter w = new StringWriter();
				Minifier childMinifier = new Minifier(w);
				
				JSONParser.parse(childMinifier, elem);
				
				resultWriter.write(w.toString());
			}
			
			resultWriter.write(character);
		} catch (IOException e) {
			throw new IllegalStateException("Could not write to given writer", e);
		}
	}
	
	/**
	 * Registers a character to recurse over, if the char is not ','
	 */
	public void elemStarted(int index, char character)
				throws IllegalStateException {
		try {
			if (character != ',')
				resultWriter.write(character);
		} catch (IOException e) {
			throw new IllegalStateException("Could not write to given writer", e);
		}
	}
	
	/**
	 * Registers a character to recurse over
	 */
	public void charRead(int index, char character) {
		elementBuilder.append(character);
	}
	
	/**
	 * <p>
	 * Writes the current key to the writer, as well as the character parameter.
	 * </p><p> 
	 * This tends to assume the key is not an array or object.
	 * </p>
	 */
	public void keyValueSeparation(int colonIndex, char character)
				throws IllegalStateException {
		try {
			resultWriter.write(elementBuilder.toString().trim());
			resultWriter.write(character);
			elementBuilder.setLength(0);
		} catch (IOException e) {
			throw new IllegalStateException("Could not write to given writer", e);
		}
	}
	
	
	
	
	
	/**
	 * Returns true if this should not recurse into the thing represented by str
	 */
	private boolean isPrimitive(String str) {
		if (str.equalsIgnoreCase("null")) return true;
		if (str.equalsIgnoreCase("true")) return true;
		if (str.equalsIgnoreCase("false")) return true;
		if (JSONString.isValid(str)) return true;
		try {
			Double.parseDouble(str);
			
			return true;
		} catch (NumberFormatException e1) {
			return false;
		}
	}
}
