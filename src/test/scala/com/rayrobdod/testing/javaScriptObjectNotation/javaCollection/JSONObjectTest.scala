package com.rayrobdod.testing.javaScriptObjectNotation.javaCollection

import com.rayrobdod.javaScriptObjectNotation.javaCollection.JSONObject
import com.rayrobdod.javaScriptObjectNotation.JSONString
import org.scalatest.{FunSuite, FunSpec}
import org.scalatest.prop.PropertyChecks
import java.text.ParseException;

/**
 * @author Raymond Dodge
 * @version 2013 Jun 25
 */
class JSONObjectTest extends FunSpec
{
	implicit def intToJSONString(x:Int):JSONString = {
		JSONString.generateParsed(x.toString)
	}
	
	describe ("A JSON Object") {
		describe ("when empty") {
			it ("claims to be empty") {
				assert( new JSONObject().isEmpty() )
			}
			it ("has a size of zero") {
				expectResult(0)( new JSONObject().size() )
			}
			it ("errors when getting") {
				intercept[IndexOutOfBoundsException]( new JSONObject().get(0) )
			}
			it ("has an unparsed value of `{}`") {
				expectResult("{}")( new JSONObject().getUnparsed() )
			}
		}
		
		describe ("with a single value added") {
			
			val value = 4
			val key = JSONString.generateParsed("a")
			def singleValueSeq = {
				val seq = new JSONObject();
				seq.put(key, value);
				seq
			}
			
			
			it ("claims to be not empty") {
				assert(! singleValueSeq.isEmpty() )
			}
			it ("has a size of one") {
				expectResult(1)( singleValueSeq.size() )
			}
			it ("returns the value when getting the key") {
				expectResult(value)( singleValueSeq.get(key) )
			}
			it ("returns null when getting a different thing") {
				expectResult(null)( singleValueSeq.get(1) )
			}
			it ("returns true when inquiring containsKey with that key") {
				expectResult(true)( singleValueSeq.containsKey(key) )
			}
			it ("returns false when inquiring containsKey with a different key") {
				expectResult(false)( singleValueSeq.containsKey(value) )
			}
			it ("returns true when inquiring containsValue with that value") {
				expectResult(true)( singleValueSeq.containsValue(value) )
			}
			it ("returns false when inquiring containsValue with a different value") {
				expectResult(false)( singleValueSeq.containsValue(key) )
			}
			it ("has an unparsed value of `{\"a\":4}`") {
				expectResult("{\"a\":4}")( singleValueSeq.getUnparsed() )
			}
		}
		
		describe ("with many values added") {
			
			def size = 20
			def manyValueSeq = {
				val seq = new JSONObject();
				(0 until size).foreach{ x => 
//					key = JSONString.generateParsed(x.toString)
					seq.put(x,x)
				}
				seq
			}
			
			it ("claims to be not empty") {
				assert(! manyValueSeq.isEmpty() )
			}
			it ("has a size of size") {
				expectResult(size)( manyValueSeq.size() )
			}
			it ("returns the value when getting 0") {
				(0 until size).foreach{ i => 
					expectResult(i)( manyValueSeq.get(intToJSONString(i)) )
				}
			}
			it ("has a correct unparsed value") {
				expectResult(
					(1 until size).foldLeft("{\"0\":0"){(s,i) => s + ",\""+i+"\":"+i} + "}"				)( 
					manyValueSeq.getUnparsed()
				)
			}
		}
		
		// This collection does not support remove, apparently
		describe ("with many values added, then a remove") {
			
			def size = 20
			def keyToRemove:JSONString = 10
			def manyValueSeq = {
				val seq = new JSONObject();
				(0 until size).foreach{ x => 
//					key = JSONString.generateParsed(x.toString)
					seq.put(x,x)
				}
				seq.remove(keyToRemove)
				seq
			}
			
			ignore ("claims to be not empty") {
				assert(! manyValueSeq.isEmpty() )
			}
			ignore ("has a size of size-1") {
				expectResult(size-1)( manyValueSeq.size() )
			}
			ignore ("returns the value when getting 0") {
				(0 until 20).foreach{ i => 
					val key = intToJSONString(i)
					if (key == keyToRemove) {
						expectResult(null)( manyValueSeq.get(key) )
					} else {
						expectResult(i)( manyValueSeq.get(key) )
					}
				}
			}
		}
		
		// or replace...
		describe ("with many values added, then a replacement") {
			
			def size = 20
			def keyToReplace:JSONString = 10
			def value = true
			def manyValueSeq = {
				val seq = new JSONObject();
				(0 until size).foreach{ x => 
//					key = JSONString.generateParsed(x.toString)
					seq.put(x,x)
				}
				seq.put(keyToReplace, value)
				seq
			}
			
			ignore ("claims to be not empty") {
				assert(! manyValueSeq.isEmpty() )
			}
			ignore ("has a size of size") {
				expectResult(size)( manyValueSeq.size() )
			}
			ignore ("returns the value when getting 0") {
				(0 until 20).foreach{ i => 
					val key = intToJSONString(i)
					if (key == keyToReplace) {
						expectResult(value)( manyValueSeq.get(key) )
					} else {
						expectResult(i)( manyValueSeq.get(key) )
					}
				}
			}
		}
		
	}
}
