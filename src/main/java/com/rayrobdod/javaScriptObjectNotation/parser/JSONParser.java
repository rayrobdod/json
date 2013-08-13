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

package com.rayrobdod.javaScriptObjectNotation.parser;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.text.ParseException;
import java.util.regex.Pattern;

/**
 * This is the class that parses Strings and calls functions in JSONParseListeners based on what it finds.
 * 
 * It will work when the string represents an Array or Object, but nothing else.
 * 
 * @author Raymond Dodge
 * @version Sep 1, 2010 to Sep 7, 2010 - Modified parser from JSONArray, then extracted it.
 * @version Sep 9, 2010 - fixing indexes returned, which were off by one
 * @version Oct 2, 2010 - modifying to allow starting anywhere
 * @version Dec 30, 2010 - removing condition that parsing ends if the reader is not ready.
 * @version 15 Dec 2011 - moved from {@code net.verizon.rayrobdod.javaScriptObjectNotation} to {@code com.rayrobdod.javaScriptObjectNotation;}
 * @version 14 Apr 2012 - making default constructor private 
 * @see JSONParseListener
 */
public final class JSONParser
{
	private JSONParser() {}
	
	@SuppressWarnings("unused")
	private static final Pattern whiteSpace = Pattern.compile("\\s");
	private static final Pattern stringBegin = Pattern.compile("\"");
	private static final Pattern stringEnd = Pattern.compile("\"");
	private static final Pattern levelUp = Pattern.compile("[\\[{]");
	private static final Pattern levelDown = Pattern.compile("[\\]}]");
	private static final Pattern elementSeperator = Pattern.compile(",");
	private static final Pattern keyValueSeperator = Pattern.compile(":");
	private static final Pattern termination = Pattern.compile("[\uFFFF]");
	// TRYTHIS figure out what other chars count as termination (EOF?)

	/**
	 * Parses a string to a JSONParseListener
	 * 
	 * @param l the ParseListener that recieves events from this parser
	 * @param s a string to create a {@link StringReader} around
	 * @throws NullPointerException if either l or reader is null
	 * @throws ParseException if the parsing fails
	 */
	public static void parse(JSONParseListener l, String s)
			throws NullPointerException, ParseException
	{
		try
		{
			parse(l, new StringReader(s));
		}
		catch (IOException e)
		{
			String message = "StringReader threw an IOException";
			AssertionError e1 = new AssertionError(message);
			e1.initCause(e);
			throw e1;
		}
	}
	
	/**
	 * Parses the specified String's contents with the specified ParseListener
	 * after skipping the specified number of characters.
	 * 
	 * @param l the ParseListener that recieves events from this parser
	 * @param s a string to create a {@link StringReader} around
	 * @param skipChars the number of characters to skip in the reader before starting. This assumes any value after zero is after the initial '{' or '['.
	 * @throws NullPointerException if either l or reader is null
	 * @throws ParseException if the parsing fails
	 */
	public static void parse(JSONParseListener l, String s, int skipChars)
			throws NullPointerException, ParseException
	{
		try
		{
			parse(l, new StringReader(s), skipChars);
		}
		catch (IOException e)
		{
			String message = "StringReader threw an IOException";
				AssertionError e1 = new AssertionError(message);
			e1.initCause(e);
			throw e1;
		}
	}

	/**
	 * Parses the specified Reader's contents with the specified ParseListener.
	 * 
	 * @param l the ParseListener that recieves events from this parser
	 * @param reader the reader that this will read chars from. It is assumed to be at the beginning of the stream
	 * @throws NullPointerException if either l or reader is null
	 * @throws ParseException if the parsing fails
	 * @throws IOException if the reader could not be read from
	 */
	public static void parse(JSONParseListener l, Reader reader)
			throws NullPointerException, ParseException, IOException
	{
		parse(l,reader,0);
	}
	
	/**
	 * This starts by firing <var>l</var>'s {@link JSONParseListener#started() started()}
	 * method. This will then take <var>reader</var>, skip <var>skipChars</var>, then
	 * silently read until it reaches a comma or bracket. Begining there it will fire an
	 * {@link JSONParseListener#openingBracket(int, char) openingBracket(int, char)}
	 * if appropriate and an
	 * {@link JSONParseListener#elemStarted(int, char) elemStarted(int, char)}
	 * on <var>l</var>. It will continue reading characters
	 * and firing appropriate events until <var>reader</var> runs out of characters
	 * or <var>l</var>'s {@link JSONParseListener#abort() abort()} method returns true.
	 * Then {@link JSONParseListener#ended() ended()} is called and the method returns.
	 * <p>
	 * This neither throws exceptions nor performs any logic at all. Logic includes
	 * determining that a list or element is empty or leaving out whitespace.
	 * Listeners can handle such cases individually.
	 * 
	 * @param l the ParseListener that recieves events from this parser
	 * @param reader the reader that this will read chars from. It is assumed to be at the beginning of the stream
	 * @param skipChars the number of characters to skip in the reader before starting. This assumes any value after zero is after the initial '{' or '['.
	 * @throws NullPointerException if either l or reader is null
	 * @throws ParseException if the parsing fails
	 * @throws IOException if the reader could not be read from
	 */
	public static void parse(JSONParseListener l, Reader reader, int skipChars)
			throws NullPointerException, ParseException, IOException
	{
		/**
		 * If this is true, the object to read has not started yet.
		 * Else, this is false.
		 */
		boolean hasStarted = false;
		/**
		 * If this is zero, read chars, the current level is the level to read.
		 * If this is -1, the current level to read has not started yet. Errors should be thrown if non-whitespace is found
		 * If this is greater than zero, a nested array is being read from; ignore any commands other than level changing events
		 * This should never be less than -1.
		 */
		int level = 0;
		/**
		 * If this is false, read formatting.
		 * If this is true, it is reading a JSONString, and should ignore formatting excepting a string end character.
		 */
		boolean isInsideQuotes = false;
		int currentIndex = skipChars - 1 /* -1 so first increment makes them equal */;
		
		reader.skip(skipChars);
		
		l.started();
		
		char currentChar;
		while (!l.abort() && !termination.matcher(Character.toString(
						currentChar = (char) reader.read())).matches())
		{
//			char currentChar = (char) reader.read();
			String currentStr = Character.toString(currentChar);
			currentIndex++;

			if (!hasStarted)
			{
				if (levelUp.matcher(currentStr).matches())
				{
					l.openingBracket(currentIndex, currentChar);
				}
				if (levelUp.matcher(currentStr).matches() ||
						elementSeperator.matcher(currentStr).matches())
				{
					hasStarted=true;
					level=0;
					l.elemStarted(currentIndex, currentChar);
				}
			}
			else if (isInsideQuotes)
			{
				l.charRead(currentIndex, currentChar);
				
				if (stringEnd.matcher(currentStr).matches())
				{
					// string has ended
					isInsideQuotes = false;
				}
			}
			else if (level > 0)
			{
				l.charRead(currentIndex, currentChar);
				
				if (stringBegin.matcher(currentStr).matches())
				{
					isInsideQuotes = true;
				}
				else if (levelUp.matcher(currentStr).matches())
				{
					level++;
				}
				else if (levelDown.matcher(currentStr).matches())
				{
					level--;
				}
			}
			else if (level < 0)
			{
				if (levelUp.matcher(currentStr).matches())
				{
					level++;
					
					if (level == 0)
					{
						l.openingBracket(currentIndex,currentChar);
						l.elemStarted(currentIndex,currentChar);
					}
				}
				else
				{
					// do nothing
				}
			}
			else // level is zero and not inside quotes
			{
				if (stringBegin.matcher(currentStr).matches())
				{
					l.charRead(currentIndex, currentChar);
					isInsideQuotes = true;
				}
				else if (levelUp.matcher(currentStr).matches())
				{
					l.charRead(currentIndex, currentChar);
					level++;
				}
				else if (levelDown.matcher(currentStr).matches())
				{
					l.elemEnded(currentIndex, currentChar);
					level--;
					l.endingBracket(currentIndex, currentChar);
				}
				else if (elementSeperator.matcher(currentStr).matches())
				{
					l.elemEnded(currentIndex, currentChar);
					l.elemStarted(currentIndex, currentChar);
				}
				else if (keyValueSeperator.matcher(currentStr).matches())
				{
					l.keyValueSeparation(currentIndex, currentChar);
				}
				else // is not a keyword
				{
					l.charRead(currentIndex, currentChar);
				}
			}
		}
		
		l.ended();
	}
}