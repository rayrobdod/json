package com.rayrobdod.javaScriptObjectNotation.javaCollection;

import java.text.ParseException;
import java.util.AbstractSequentialList;
import java.util.ArrayList;
import java.util.Collection;
import java.util.ConcurrentModificationException;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.NoSuchElementException;
import com.rayrobdod.javaScriptObjectNotation.parser.listeners.GetElementBounds;
import com.rayrobdod.javaScriptObjectNotation.parser.listeners.GetArrayElement;
import com.rayrobdod.javaScriptObjectNotation.parser.listeners.JSONArrayValidator;
import com.rayrobdod.javaScriptObjectNotation.parser.JSONParseAdapter;
import com.rayrobdod.javaScriptObjectNotation.parser.JSONParseListener;
import com.rayrobdod.javaScriptObjectNotation.parser.JSONParser;
import com.rayrobdod.javaScriptObjectNotation.parser.listeners.GetSize;
import com.rayrobdod.javaScriptObjectNotation.javaCollection.parseListeners.NextJSONArrayParseListener;
import com.rayrobdod.javaScriptObjectNotation.JSONString;
import static com.rayrobdod.javaScriptObjectNotation.javaCollection.JSONObject.encode;
import com.rayrobdod.javaScriptObjectNotation.parser.decoders.ToJavaObjectJSONDecoder;

/**
 * <p>
 * This is an array in the JSON language.
 * <p>
 * Unlike other versions of JSONArrays I have encountered,
 * this does not just store the information in a
 * class that extends {@link ArrayList}, or worse, uses one
 * and not implement {@link List}. This extends
 * {@link AbstractSequentialList}, but only one class variable,
 * a {@link CharSequence}, so this is completly unrelated to
 * ArrayLists.
 * <p>
 * If you would like the speed of a diferent type of list, this
 * does implement a {@link #JSONArray(Collection) Collection constructor},
 * and most other {@link Collection}s have a Collection
 * constructor, so different implementations of
 * {@link List} can be changed between at will.
 * 
 * @author Raymond Dodge
 * @version 18 June 2010 - 1.0
 * @version Aug 29, 2010 - fixing iterator's hasNext() method
 * @version Aug 29, 2010 - {@link #isValid(String)} no longer accepts empty strings
 * @version Aug 29, 2010 - fixing iterator's hasNext() method even more
 * @version Aug 29, 2010 - tried fixing parser's layered abilities; failed horibly and gave up
 * @version Sep 9, 2010 - changing several CharSequence methods and object to Strings
 * @version Sep 9, 2010 - major - making use {@link JSONParseListener} classes instead of internal one
 * @version 15 Dec 2011 - moved from {@code net.verizon.rayrobdod.javaScriptObjectNotation} to {@code com.rayrobdod.javaScriptObjectNotation;}
 * @version 14 Jan 2012 - moved from {@code com.rayrobdod.javaScriptObjectNotation} to {@code com.rayrobdod.javaScriptObjectNotation.javaCollection;}
 * @version 14 Jan 2012 - extracted com.rayrobdod.javaScriptObjectNotation.parseListeners.NextJSONArrayParseListener and com.rayrobdod.javaScriptObjectNotation.JSONArrayIterator
 * @version 18 Jan 2012 - deprecated {@link #isValid}
 * @version 2013 Jun 23 - using ToJavaObjectJSONDecoder.decode instad of JSONObject.decode
 * @version 2013 Jun 23 - fixing a few instances of off-by-one errors relating to commas
 */
public final class JSONArray extends AbstractSequentialList<Object>
{
	private String unparsed;
	
	/**
	 * Creates an empty JSONArray
	 */
	public JSONArray()
	{
		this.clear();
	}
	
	/**
	 * Creates a JSONArray based on the string
	 * @param unparsedString the string to decode into a JSONArray
	 * @throws ParseException if unparsedString is invalid
	 * @throws NullPointerException if unparsedString is null
	 */
	public JSONArray(String unparsedString) throws ParseException,
			NullPointerException
	{
		this();
		if (unparsedString == null) throw new NullPointerException("Parameter was null");
		this.unparsed = unparsedString;
		
		JSONParser.parse(new JSONArrayValidator(), unparsed); // error throwing
	}
	
	/**
	 * Creates a JSONArray that contains the same objects as the collection
	 * @param c the collection to copy.
	 * @throws NullPointerException if c is null
	 */
	public JSONArray(Collection<?> c) throws NullPointerException
	{
		this();
		this.addAll(c);
	}
	
	/**
	 * returns a charsequence containing the unparsed verion of this array
	 * @return a charsequence containing the unparsed verion of this array
	 */
	public CharSequence getUnparsed()
	{
		return this.unparsed;
	}
	
	public ListIterator<Object> listIterator(int index)
	{
		if (index < 0 || index > this.size())
		{
			throw new IndexOutOfBoundsException();
		}
		
		return new JSONArrayIterator(this.unparsed, index);
	}
	
	/**
	 * returns the number of elements in this JSONArray.
	 * @return the number of elements in this JSONArray.
	 */
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
/*		catch (IOException e)
		{
			AssertionError e1 = new AssertionError("StringReader threw an IOException");
			e1.initCause(e);
			throw e1;
		}
*/		
		return l.getCount();
	}
	
	/**
	 * This removes all the objects in the JSONArray.
	 */
	public void clear()
	{
		unparsed = "[]";
		modCount++;
	}
	
	/**
	 * returns true if the list is empty
	 * @return true if <code>{@link #size()} == 0</code>
	 */
	public boolean isEmpty()
	{
		return size() == 0;
	}
	
	/**
	 * Adds the specified Object to the JSONArray. Due to differences between JSON
	 * and Java objects, only certain types of objects can be added. Additionally,
	 * these objects will be converted into a corresponding, equivalent JSON class
	 * before being stored.
	 * <p>
	 * The list of conversions is:
	 * <ul>
			<li>{@link Map} to {@link JSONObject}</li>
			<li>{@link Collection} to {@link JSONArray}</li>
			<li>{@link CharSequence} to {@link JSONString}</li>
			<li>{@link Number} to {@link Double} or {@link Long}</li>
	 * </ul>
	 * {@link Boolean} and {@code null} suffer no data loss in the conversion. All
	 * other data types are unable to be added.
	 * 
	 * @param o the Object to add.
	 * @return true. Always.
	 * @throws ClassCastException if the object is not one of the following types:
	 * <ul>
			<li>the null object</li><li>a {@link Boolean}</li>
			<li>a {@link Number}</li><li>a {@link CharSequence}</li>
			<li>a {@link Collection}</li><li>a {@link Map}</li>
	 * </ul>
	 * 
	 */
	public boolean add(Object o) throws ClassCastException
	{
		if (this.isEmpty())
		{
			unparsed = "[" + encode(o) + "]";
		}
		else
		{
			int splitIndex = unparsed.lastIndexOf(']');

			unparsed = unparsed.subSequence(0, splitIndex).toString()
					+ ',' + encode(o) + ']';
		}

		modCount++;
		return true;
	}
	
	/**
	 * Adds the specified element at the specified index
	 * @param index the index to add the element at
	 * @param o the object to add
	 * @throws IndexOutOfBoundsException if the index to add is out of the list's bounds
	 */
	public void add(final int index, final Object o) throws IndexOutOfBoundsException
	{
		if (index == size())
		{
			add(o);
			return;
		}
		
		if (index < 0 || index >= size()) throw new IndexOutOfBoundsException(
				index + "is not in range of [0," + size() + "]");
		
		GetElementBounds l = new GetElementBounds(index);
		
		try
		{
			JSONParser.parse(l, unparsed);
		}
		catch (ParseException e)
		{
			throw new AssertionError(e);
		}
		
		unparsed = unparsed.substring(0, l.getElementStartIndex())
				+ ',' + encode(o)
				+ unparsed.substring(l.getElementStartIndex());

		modCount++;
	}

	/**
	 * Returns the element at the specified position in this list.
	 *  
	 * @param index the index of the value to get
	 * @return the null, Boolean, Double, Long, JSONString, JSONArray or JSONObject
	 * that was set at this particular index. 
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public Object get(final int index) throws IndexOutOfBoundsException
	{
		if (index < 0 || index >= size()) throw new IndexOutOfBoundsException(
				index + "is not in range of [0," + size() + ")");
		
		GetArrayElement l = new GetArrayElement(index); 
		
		try
		{
			JSONParser.parse(l, this.unparsed);
		}
		catch (ParseException e)
		{
			throw new AssertionError(e);
		}
		
		return l.getParsedElement();
	}
	
	public Object set(final int index, final Object o)
			throws ClassCastException, IndexOutOfBoundsException
	{
		if (index < 0 || index >= size()) throw new IndexOutOfBoundsException(
				index + "is not in range of [0," + size() + "]");
		
		GetElementBounds l = new GetElementBounds(index);
		
		try
		{
			JSONParser.parse(l, unparsed);
		}
		catch (ParseException e)
		{
			throw new AssertionError(e);
		}
		
		String removed = unparsed.substring(l.getElementStartIndex() + 1, l.getElementEndIndex() - 1
				/* -1 to exclude comma */);
		unparsed = unparsed.substring(0, l.getElementStartIndex() + 1)
				+ encode(o)
				+ unparsed.substring(l.getElementEndIndex());

		modCount++;
		return ToJavaObjectJSONDecoder.instance.decode(removed);
	}
	
	public Object remove(final int index)
	{
		GetElementBounds l = new GetElementBounds(index);
		
		try
		{
			JSONParser.parse(l, unparsed);
		}
		catch (ParseException e)
		{
			throw new AssertionError(e);
		}
		
		String removed = unparsed.substring(l.getElementStartIndex() + 1, l.getElementEndIndex() - 1
				/* -1 to exclude comma */);
		unparsed = unparsed.substring(0, l.getElementStartIndex()) +
				unparsed.substring(l.getElementEndIndex());
		
		modCount++;
		return ToJavaObjectJSONDecoder.instance.decode(removed);
	}
	
	/**
	 * Tests a string to make sure it is formatted correctly.
	 * 
	 * @param unparsedString the string to test
	 * @return true if {@link #JSONArray(String)} can execute without errors
	 * using the specified CharSequence
	 * @deprecated use {@link com.rayrobdod.javaScriptObjectNotation.parser.decoders.isValid#jsonArray} instead
	 */
	public static boolean isValid(String unparsedString)
	{
		if (unparsedString.length() == 0) return false;
		try
		{
			JSONParser.parse(new JSONArrayValidator(), unparsedString);
			
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
/*		catch (IOException e)
		{
			return false;
		}
*/
	}
}
