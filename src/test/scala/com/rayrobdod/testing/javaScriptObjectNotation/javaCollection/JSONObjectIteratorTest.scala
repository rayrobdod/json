package com.rayrobdod.testing.javaScriptObjectNotation.javaCollection

import com.rayrobdod.javaScriptObjectNotation.javaCollection.{
		JSONObject, JSONObjectIterator, JSONObjectEntry
}
import com.rayrobdod.javaScriptObjectNotation.JSONString
import java.util.Map.Entry;
import org.scalatest.{FunSuite, FunSpec}
import org.scalatest.prop.PropertyChecks
import java.text.ParseException;
import scala.collection.JavaConversions.asScalaIterator

/**
 * @author Raymond Dodge
 * @since 2013 Sept 15
 */
class JSONObjectIteratorTest extends FunSpec
{
	implicit def intToJSONString(x:Int):JSONString = {
		JSONString.generateParsed(x.toString)
	}
	
	describe ("A JSON Object Iterator") {
		describe ("with zero values") {
			def empty = new JSONObjectIterator("{}");
			
			it ("does not have a next") {
				assert( !empty.hasNext() )
			}
			it ("errors when trying to get next") {
				intercept[NoSuchElementException]( empty.next() )
			}
			it ("errors when trying to remove") {
				intercept[IllegalStateException]( empty.remove() )
			}
		}
		
		describe ("with a single value") {
			def myThing1 = new JSONObjectIterator("""{"hi":1}""");
			def myThing0 = { val a = myThing1; a.next(); a}
			
			it ("does have a next") {
				assert( myThing1.hasNext() )
			}
			it ("has a next with the correct key") {
				expectResult("hi"){myThing1.next().getKey.toString}
			}
			it ("has a next with the correct value") {
				expectResult(1){myThing1.next().getValue}
			}
			it ("cannot remove before next") {
				intercept[IllegalStateException]( myThing1.remove() )
			}
			
			it ("does not have a second next") {
				assert( !myThing0.hasNext() )
			}
			it ("errors when trying to get second next") {
				intercept[NoSuchElementException]( myThing0.next() )
			}
			ignore ("somethingsomething related to remove...") {
				intercept[IllegalStateException]( myThing0.remove() )
			}
		}
		
		describe ("with many values") {
			
			val size = 20
			def manyValueSeq = {
				val seq = new JSONObject();
				(0 until size).foreach{ x => 
					seq.put(x,x)
				}
				seq
			}
			def iterator = manyValueSeq.entrySet.iterator;
			
			it ("iterates through the things in order (IE, is an order-preserving map)") {
				(iterator zip (0 until size).iterator).foreach({(j:Entry[JSONString, Object], i:Int) =>
					expectResult(i){j.getValue}
					expectResult(i.toString){j.getKey.toString}
				}.tupled)
			}
		}
		
	}
}
