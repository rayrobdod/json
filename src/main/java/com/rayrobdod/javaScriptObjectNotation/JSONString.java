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

package com.rayrobdod.javaScriptObjectNotation;

import java.text.ParseException;
import java.util.EventListener;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import javax.swing.text.Segment;

/**
 * <p>
 * This is a class that parses and encodes JSONStrings.
 * </p><p>
 * All methods inherited by {@link CharSequence} act on the parsed version
 * of the string.
 * </p>
 * @author Raymond Dodge
 * @version Mar 13, 2010
 * @version June 28, 2010 - increased consistency of equals and hashcode.
 * @version Aug 15, 2010 - made {@link #equals(Object)} and {@link #hashCode()} evaluate on the parsed string, not the unparsed string
 * @version Aug 29, 2010 - {@link #isValid(CharSequence)} no longer accepts empty strings
 * @version 15 Dec 2011 - moved from {@code net.verizon.rayrobdod.javaScriptObjectNotation} to {@code com.rayrobdod.javaScriptObjectNotation;}
 * @version 2013 Jun 22 - making certain methods throw ParseExceptions instead of IndexOutOfBounds or NumberFormatException 
 * @version 2013 Jun 22 - removing dependency on {@link com.rayrobdod.util.WrappedObject}
 * @version 2013 Jun 22 - unparsing of strings will now escape Unicode control characters, as it's supposed to.
 * @version 2013 Jun 22 - fixing iterator to not derp when the unparsed string isn't trimmed.
 * @version 2013 Jun 22 - using getEscapeToCharacters in iterator, as opposed to repetitive 'elseif's
 * @todo minimize size
 */
public final class JSONString implements CharSequence, Comparable<JSONString>, Iterable<Character>
{
	private CharSequence unparsed;
	
	/**
	 * Constructs a JSONString that is equivalent to the given CharSequence.
	 * This means that for all i in <code>0&lt;i&lt;c.length()</code>,
	 * <code>generateParsed(c).charAt(i) == c.charAt(i)</code>
	 * 
	 * @param c the CharSequence to copy
	 * @return a JSONString equal to c
	 */
	public static final JSONString generateParsed(CharSequence c)
	{
		try
		{
			return new JSONString(encode(c));
		}
		catch (ParseException e)
		{
			throw new AssertionError(e);
		}
	}
	
	/**
	 * This generates a JSONString from a raw CharSequence. This will parse c, and
	 * be equivalent to the result.
	 * @param c the CharSequence to parse
	 * @return a parsed JSONString
	 * @throws ParseException if c is not a properly formed JSON string.
	 */
	public static final JSONString generateUnparsed(CharSequence c) throws ParseException
	{
		return new JSONString(c);
	}
	
	/**
	 * Creates a JSONString from the specified unparsedString
	 * @param unparsedString the string to build this from
	 * @throws ParseException if the string is malformed.
	 */
	protected JSONString(CharSequence unparsedString) throws ParseException
	{
		// this will throw an error if there is one
		parse(new ParseAdapter(), unparsedString);
		
		this.unparsed = unparsedString.toString();
	}

	public char charAt(final int index) throws IndexOutOfBoundsException
	{
		class CharAtParseListener implements ParseListener
		{
			private int currentIndex = -1;
			private char retVal;
			private boolean hasRetVal = false;
			
			public boolean abort()
			{
				return hasRetVal;
			}

			public void charRead(char character)
			{
				currentIndex++;
				
				if (currentIndex == index)
				{
					hasRetVal = true;
					retVal = character;
				}
			}
			
			public void ended() {}
			
			public boolean hasResult() {return hasRetVal;}
			
			public char result() { return retVal; }
		}
		
		try
		{
			CharAtParseListener listener = new CharAtParseListener();
			this.parse(listener);
			
			if (listener.hasResult())
			{
				return listener.result();
			}
			else
			{
				throw new IndexOutOfBoundsException("index: " + index);
			}
		}
		catch (ParseException e)
		{
			throw new AssertionError(e);
		}
	}
	
	public int length()
	{
		class LengthParseListener implements ParseListener
		{
			int index = 0;
			
			public boolean abort()
			{
				return false;
			}

			public void charRead(char character)
			{
				index++;
			}

			public void ended() {}
			
			public int retVal() { return index; }
		}
		
		try
		{
			LengthParseListener l = new LengthParseListener();
			parse(l);
			return l.retVal();
		}
		catch (ParseException e)
		{
			throw new AssertionError(e);
		}
	}
	
	public CharSequence subSequence(int start, int length)
	{
		return new Segment(this.toString().toCharArray(), start, length);
	}
	
	public int compareTo(JSONString other)
	{
		Iterator<Character> thisIterator = this.iterator();
		Iterator<Character> otherIterator = other.iterator();
		
		while (thisIterator.hasNext() && otherIterator.hasNext())
		{
			Character thisChar = thisIterator.next();
			Character otherChar = otherIterator.next();
			
			if (!thisChar.equals(otherChar))
			{
				return (thisChar.compareTo(otherChar));
			}
		}
		
		if (thisIterator.hasNext())
		{
			return 1;
		}
		else if (otherIterator.hasNext())
		{
			return -1;
		}
		else
		{
			return 0;
		}
	}
	
	public Iterator<Character> iterator()
	{
		class JSONStringIterator implements Iterator<Character>
		{
			int currentIndex = unparsed.toString().indexOf('"');
			Map<Character, Character> codes = getEscapeToCharacters();
			
			public boolean hasNext()
			{
				return (unparsed.charAt(currentIndex + 1) != '"');
			}

			public Character next() throws NoSuchElementException
			{
				currentIndex++;
				int currentChar = unparsed.charAt(currentIndex);
				int secondChar = (currentIndex + 1 < unparsed.length() ? 
						unparsed.charAt(currentIndex + 1) : -1);
				
				if (currentChar == '"')
				{
					throw new NoSuchElementException();
				}
				else if (currentChar == '\\')
				{
					if (codes.containsKey(new Character((char) secondChar)))
					{
						currentIndex++;
						return codes.get((char) secondChar);
					}
					else if (secondChar == 'u')
					{
						CharSequence hexcode = unparsed.subSequence(currentIndex + 2,
								currentIndex + 2 + 4);
						
						char readChar = (char) Integer.parseInt(hexcode.toString(), 16);

						currentIndex += 5;
						return readChar;
					}
					else
					{
						throw new NoSuchElementException();
					}
				}
				else
				{
					return (char) currentChar;
				}
			}

			public void remove()
			{
				throw new UnsupportedOperationException();
			}
			
		}
		
		return new JSONStringIterator();
	}
	
	/**
	 * Determines if the string is a valid string
	 * @param unparsed
	 * @return true if the unparsed string can parse without exceptions
	 */
	public static boolean isValid(CharSequence unparsed)
	{
		if (unparsed.length() == 0) return false;
		try
		{
			parse(new ParseAdapter(), unparsed);
			
			return true;
		}
		catch (ParseException e)
		{
//			e.printStackTrace();
			
			return false;
		}
	}
	
	/**
	 * returns the unparsed version of this string
	 * @return the unparsed version of this string
	 */
	public String getUnparsedString()
	{
		return unparsed.toString();
	}
	
	public String toString()
	{
		class ToStringParseListener implements ParseListener
		{
			public StringBuilder retVal = new StringBuilder();
			
			public boolean abort()
			{
				return false;
			}

			public void charRead(char character)
			{
				retVal.append(character);
			}

			public void ended() {}
			
			public String result() {
				return retVal.toString();
			}
		}
		
		try
		{
			ToStringParseListener l = new ToStringParseListener(); 
			parse(l);
			return l.result();
		}
		catch (ParseException e)
		{
			throw new AssertionError(e);
		}
	}

	
	/**
	 * This is a listener used by {@link #parse(ParseListener)} to
	 * notify about interesting events.
	 */
	protected interface ParseListener extends EventListener
	{
		/**
		 * called each time is character is read
		 * @param character the read character
		 */
		void charRead(char character);
		/**
		 * called to determine if parsing should end
		 * @return true if the parsing should end
		 */
		boolean abort();
		/**
		 * called when parsing ends
		 */
		void ended();
	}
	
	/** an implementation of Parselistener that does nothing */
	protected static class ParseAdapter implements ParseListener
	{
		public boolean abort()
		{
			return false;
		}

		public void charRead(char character)
		{
			// do nothing
		}

		public void ended()
		{
			// do nothing
		}
	}
	
	/**
	 * Parses the specified string
	 * @param l the listener that is told what to do upon interesting events
	 * @param string the string to parse
	 * @throws ParseException if an exception occurs parsing the string
	 * @throws NullPointerException if the listener is null
	 */
	protected static void parse(ParseListener l, CharSequence string)
			throws ParseException, NullPointerException
	{
		string = string.toString().trim();
		
		if (string.length() == 0)
		{
			throw new ParseException("The string was empty", 0);
		}
		if (string.charAt(0) != '"')
		{
			throw new ParseException("the first character of the string was not a '\"'", 0);
		}
		else if (string.charAt(string.length() - 1) != '"')
		{
			throw new ParseException("the last character of the string was not a '\"'", string.length() - 1);
		}
		if (string.length() == 1)
		{
			throw new ParseException("The string did not contain two '\"'s", 0);
		}

		Map<Character, Character> codes = getEscapeToCharacters();
		
		int currentIndex = 1;
		int currentChar = -1;
		int secondChar = -1;
		
		while (currentIndex < string.length() - 1 && !l.abort())
		{
			currentChar = string.charAt(currentIndex);
			secondChar = (currentIndex + 1 < string.length() ? 
					string.charAt(currentIndex + 1) : -1);
			
			if (currentChar == '"')
			{
				throw new ParseException("all '\"' in string must be escaped",
						currentChar);
			}
			else if (currentChar == '\\')
			{
				if (codes.containsKey(new Character((char) secondChar)))
				{
					l.charRead(codes.get((char) secondChar));
					currentIndex++;
				}
				else if (secondChar == 'u')
				{
					try {
						CharSequence hexcode = string.subSequence(currentIndex + 2,
								currentIndex + 2 + 4);
						
						char readChar = (char) Integer.parseInt(hexcode.toString(), 16);
						
						l.charRead(readChar);
						currentIndex += 5;
					}
					catch (final IndexOutOfBoundsException e) {
						throw new ParseException("Invalid unicode escape: " + string.toString().substring(currentIndex + 2), currentIndex) {
							public Throwable getCause() {return e;}
						};
					}
					catch (final java.lang.NumberFormatException e) {
						throw new ParseException("Invalid unicode escape: " + string.subSequence(currentIndex + 2, currentIndex + 6), currentIndex) {
							public Throwable getCause() {return e;}
						};
					}
				}
				else
				{
					throw new ParseException("unexpected chatacter after '\\':" +
							" char: " + secondChar + "; index: " + currentIndex, currentIndex);
				}
			}
			else
			{
				l.charRead((char) currentChar);
			}

			currentIndex++;
		}
		
		l.ended();
	}
	
	/**
	 * Parses the string contained in this
	 * @param l the listener that is told what to do upon interesting events
	 * @throws ParseException if an exception occurs parsing the string
	 * @throws NullPointerException if the listener is null
	 */
	protected void parse(ParseListener l) throws ParseException, NullPointerException
	{
		parse(l, this.unparsed);
	}
	
	/**
	 * This is the inverse of parse.
	 * 
	 * @param s the CharSequence to encode
	 * @return an encoded JSONString
	 */
	protected static CharSequence encode(CharSequence s)
	{
		Map<Character, Character> codes = getCharactersToEscape(); 
		
		StringBuilder returnValue = new StringBuilder();
		int currentIndex = 0;
		
		returnValue.append('"');
		
		while (currentIndex < s.length())
		{
			char currentChar = s.charAt(currentIndex);
			
			if (codes.containsKey(currentChar))
			{
				returnValue.append("\\" + codes.get(currentChar));
			}
			else if (Character.isISOControl(currentChar))
			{
				String a = "0000" + Integer.toHexString(currentChar);
				
				returnValue.append("\\u" + a.substring(a.length() - 4));
			}
			else
			{
				returnValue.append(currentChar);
			}
			
			currentIndex++;
		}
		
		returnValue.append('"');
		
		return returnValue;
	}
	
	/**
	 * returns the map of character it represents to the escape characters.
	 * @return a map with escaped character as the key, and it's escape code as the value
	 */
	protected static Map<Character, Character> getCharactersToEscape()
	{
		Map<Character, Character> returnValue = new HashMap<Character, Character>();
		returnValue.put('\\', '\\');
		returnValue.put('/' , '/');
		returnValue.put('\"', '\"');
		returnValue.put('\b', 'b');
		returnValue.put('\f', 'f');
		returnValue.put('\n', 'n');
		returnValue.put('\r', 'r');
		returnValue.put('\t', 't');
		
		return returnValue;
	}
	
	/**
	 * returns the map of escape characters to the character it represents.
	 * @return a map with escape codes as the key, and what it represents as the value
	 */
	protected static Map<Character, Character> getEscapeToCharacters()
	{
		Map<Character, Character> charToEsca = getCharactersToEscape();
		Map<Character, Character> returnValue = new HashMap<Character, Character>();
		
		for (Map.Entry<Character, Character> e : charToEsca.entrySet())
		{
			returnValue.put(e.getValue(), e.getKey());
		}
		
		return returnValue;
		
	}
	
	protected JSONString clone() throws CloneNotSupportedException
	{
		JSONString clone = (JSONString) super.clone();
		
		clone.unparsed = this.unparsed.toString();
		
		return clone;
	}
	
	public boolean equals(Object other)
	{
		if (!(other instanceof JSONString)) return false;
		
		return (this.toString().equals(((JSONString) other).toString()));
	}
	
	public int hashCode()
	{
		return this.toString().hashCode();
	}
}
