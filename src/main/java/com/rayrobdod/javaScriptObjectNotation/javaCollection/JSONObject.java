package com.rayrobdod.javaScriptObjectNotation.javaCollection;

import java.text.ParseException;
import java.util.AbstractMap;
import java.util.AbstractSet;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import com.rayrobdod.javaScriptObjectNotation.parser.listeners.JSONObjectValidator;
import com.rayrobdod.javaScriptObjectNotation.parser.JSONParseAdapter;
import com.rayrobdod.javaScriptObjectNotation.parser.JSONParser;
import com.rayrobdod.javaScriptObjectNotation.parser.listeners.GetSize;
import com.rayrobdod.javaScriptObjectNotation.JSONString;
import static com.rayrobdod.javaScriptObjectNotation.javaCollection.JSONObject.encode;

/**
 * <p>
 * This is an Object in the JSON language.
 * </p><p>
 * Note that things put into the JSONObject may not be the things taken
 * out of the object; integer numbers will be taken out as Longs, for example.
 * </p>
 * 
 * @author Raymond Dodge
 * @version Jun 28, 2010
 * @version Aug 10, 2010 - modified {@link #decode(String)} to trim input
 * @version Aug 19, 2010 - adding ability for {@link #encode(Object)} to accept
				Maps and JSONObjects; fixing output of {@link #encode(Object)}
				for JSONArrays
 * @version Aug 21, 2010 - fixed JSONObjectEntrySetIterator.hasNext() so that it
 				will end correctly
 * @version Aug 21, 2010 - fixed JSONObjectEntrySetIterator.next() so that entries
  				do not share references to end values
 * @version Aug 29, 2010 - fixed {@link #encode(Object)} so that it does not try
				to read a JSONArray as a JSONObject
 * @version Aug 29, 2010 - {@link #isValid(String)} no longer accepts empty strings
 * @version Sep 9, 2010 - changing several CharSequence methods and object to Strings
 * @version Sep 11, 2010 - rewrote to use {@link JSONParser} instead of internal parser
 * @version Nov 22, 2010 - modified encode so that there is no UncheckedCastWarning
			when converting an arbitrary map into a JSONObject
 * @version Aug 03, 2011 - playing with the iterator...
 * @version 15 Dec 2011 - moved from {@code net.verizon.rayrobdod.javaScriptObjectNotation}
			to {@code com.rayrobdod.javaScriptObjectNotation;}
 * @version 14 Jan 2012 - moved from {@code com.rayrobdod.javaScriptObjectNotation}
			to {@code com.rayrobdod.javaScriptObjectNotation.javaCollection;}
 * @version 15 Jan 2012 - extracting JSONObjectIterator and nuking internal
			NextJSONObjectParseListener in favor of
			com.rayrobdod.javaScriptObjectNotation.javaCollection.parseListeners.NextJSONObjectParseListener
 * @version 18 Jan 2012 - deprecated {@link #isValid}
 */
public final class JSONObject extends AbstractMap<JSONString, Object>
{
	private String unparsed;
	
	/**
	 * This creates an empty JSONObject
	 */
	public JSONObject()
	{
		unparsed = "{}";
	}
	
	/**
	 * This creates a JSONObject containing the contents of other.
	 * @param other the Map to copy values from
	 */
	public JSONObject(Map<JSONString, ?> other)
	{
		this();
		this.putAll(other);
	}
	
	/**
	 * Creates a JSONObject from an unparsed CharSequence
	 * @param unparsedString the unparsed CharSequence
	 * @throws ParseException if unparsedString is impropperly formatted
	 * @throws NullPointerException if unparsedString is null
	 */
	public JSONObject(String unparsedString) throws NullPointerException,
			ParseException
	{
		this();
		if (unparsedString == null) throw new NullPointerException("Parameter was null");
		this.unparsed = unparsedString;
		
		JSONParser.parse(new JSONObjectValidator(), unparsed); // error throwing
	}
	
	/**
	 * returns a charsequence containing the unparsed verion of this object
	 * @return a charsequence containing the unparsed verion of this object
	 */
	public CharSequence getUnparsed()
	{
		return this.unparsed;
	}
	
	public Set<Map.Entry<JSONString, Object>> entrySet()
	{
		class JSONObjectEntrySet extends AbstractSet<Map.Entry<JSONString, Object>>
		{
			public Iterator<Map.Entry<JSONString, Object>> iterator()
			{
				return new JSONObjectIterator(unparsed);
			}

			public int size()
			{
				GetSize l = new GetSize();
				
				try
				{
					JSONParser.parse(l, unparsed);
				}
				catch (ParseException e)
				{
					// constructors assert that unparsed is valid
					throw new AssertionError(e);
				}
				
				return l.getCount();
			}
			
		}
		
		return new JSONObjectEntrySet();
	}

	/**
	 * Associates the specified value with the specified key in this map.
	 * If the map previously contained a mapping for the key, the old value
	 * is replaced by the specified value.
	 * @param key the key to associate a value with
	 * @param value the value to be associated
	 * @return the value previously associated with value; null if
			key had no previous assocaition
	 */
	public Object put(JSONString key, Object value)
	{
		if (this.isEmpty())
		{
			unparsed = '{' + key.getUnparsedString() + ':'
					+ JSONObject.encode(value) + '}';
			
			return null;
		}
		else if (this.containsKey(key))
		{
			Object returnValue = null;
			
			for (Entry<JSONString, Object> e : this.entrySet())
			{
				if (e.getKey().equals(key))
				{
					returnValue = e.setValue(value);
					break;
				}
			}
			
			return returnValue;
		}
		else
		{
			int finalBracketLocation = unparsed.toString().lastIndexOf('}');
			
			unparsed = unparsed.subSequence(0, finalBracketLocation).toString() + ','
					+ key.getUnparsedString() + ':' + JSONObject.encode(value) + '}';
			
			return null;
		}
	}
	
	/**
	 * Removes all of the mappings from this map.
	 */
	public void clear()
	{
		unparsed = "{}";
	}
	
	public int size()
	{
		GetSize l = new GetSize();
		
		try
		{
			JSONParser.parse(l, unparsed);
		}
		catch (ParseException e)
		{
			// constructors assert that unparsed is valid
			throw new AssertionError(e);
		}
		
		return l.getCount();
	}
	
	/**
	 * Encodes the object into an unparsed string
	 * TODO: make depreciated somehow.
	 * @param o the object
	 * @return the string representing the object.
	 * @throws ClassCastException if the object is not one of the following types:
	 * <ul>
			<li>the null object</li><li>a {@link Boolean}</li>
			<li>a {@link Number}</li><li>a {@link CharSequence}</li>
			<li>a {@link Collection}</li><li>a {@link Map}</li>
	 * </ul>
	 */
	public static String encode(Object o) throws ClassCastException
	{
		if (o == null)
		{
			return "null";
		}
		else if (o instanceof CharSequence)
		{
			return JSONString.generateParsed((CharSequence) o)
						.getUnparsedString().toString();
		}
		else if (o instanceof JSONObject)
		{
			return ((JSONObject) o).getUnparsed().toString();
		}
		else if (o instanceof Map<?,?>)
		{
			Map<?,?> o2 = (Map<?,?>) o;
			JSONObject o3 = new JSONObject();
			
			for (Entry<?,?> e : o2.entrySet())
			{
				o3.put(JSONString.generateParsed(e.getKey().toString()),
						e.getValue());
			}
			
			return o3.getUnparsed().toString();
		}
		else if (o instanceof JSONArray)
		{
			return ((JSONArray) o).getUnparsed().toString();
		}
		else if (o instanceof Collection<?>)
		{
			return new JSONArray((Collection<?>) o).getUnparsed().toString();
		}
		else if (o instanceof Number)
		{
			Number o2 = (Number) o;
			
			if (o2.longValue() == o2.doubleValue())
			{
				return Long.toString(o2.longValue());
			}
			else
			{
				return Double.toString(o2.doubleValue());
			}
		}
		else if (o instanceof Boolean)
		{
			return o.toString();
		}
		else
		{
			throw new ClassCastException(o.toString());
		}
	}
	
	/**
	 * Tests a string to make sure it is formatted correctly.
	 * 
	 * @param c the string to test
	 * @return true if {@link #JSONObject(String)} can execute without errors
	 * using the specified CharSequence
	 * @deprecated use {@link com.rayrobdod.javaScriptObjectNotation.parser.decoders.isValid#jsonObject} instead
	 */
	public static boolean isValid(String c)
	{
		if (c.length() == 0) return false;
		try
		{
			JSONParser.parse(new JSONObjectValidator(), c);
			
			return true;
		}
		catch (ParseException e)
		{
			return false;
		}
		catch (NullPointerException e)
		{
			return false;
		}
	}
}
