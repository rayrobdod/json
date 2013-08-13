package com.rayrobdod.testing.javaScriptObjectNotation

import com.rayrobdod.javaScriptObjectNotation.javaCollection.JSONArrayIterator
import org.scalatest.{FunSuite, BeforeAndAfter}
import java.text.ParseException;

/**
 * @author Raymond Dodge
 * @version 14 Jan 2012
 */
class JSONArrayIteratorTest extends FunSuite
{
	test("An empty JSON Array is parsed")
	{
		val iterator = new JSONArrayIterator("[]")
		
		assert(! iterator.hasPrevious)
		assert(! iterator.hasNext)
		intercept[NoSuchElementException] {
			iterator.next()
		}
	}
	
	test("An single element Array is parsed forwards")
	{
		val iterator = new JSONArrayIterator("[5]")
		
		assert(!iterator.hasPrevious)
		assert(iterator.hasNext)
		assert(iterator.nextIndex === 0)
		assert(iterator.next() === 5)
		assert(iterator.hasPrevious)
		assert(! iterator.hasNext)
	}
	
	test("An single element Array is parsed backwards")
	{
		val iterator = new JSONArrayIterator("[5]")
		
		assert(iterator.next() === 5)
		assert(iterator.hasPrevious)
		assert(! iterator.hasNext)
		assert(iterator.previous() === 5)
		assert(! iterator.hasPrevious)
		assert(iterator.hasNext)
	}
	
	test("An multi element Array is parsed forwards")
	{
		val iterator = new JSONArrayIterator("[1,2,3,4,5]")
		
		assert(iterator.next() === 1)
		assert(iterator.next() === 2)
		assert(iterator.next() === 3)
		assert(iterator.hasPrevious)
		assert(iterator.hasNext)
		assert(iterator.next() === 4)
		assert(iterator.next() === 5)
	}
	
	test("An multi element Array is parsed forwards from the middle")
	{
		val iterator = new JSONArrayIterator("[0,1,2,3,4,5]",3)
		
		assert(iterator.hasPrevious)
		assert(iterator.hasNext)
		assert(iterator.next() === 3)
	}
	
	test("An multi element Array is parsed backwards from the middle")
	{
		val iterator = new JSONArrayIterator("[0,1,2,3,4,5]",3)
		
		assert(iterator.hasPrevious)
		assert(iterator.hasNext)
		assert(iterator.previous() === 2)
	}
}
