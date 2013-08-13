package com.rayrobdod.testing.javaScriptObjectNotation.javaCollection

import com.rayrobdod.javaScriptObjectNotation.javaCollection.JSONArray
import org.scalatest.{FunSuite, FunSpec}
import org.scalatest.prop.PropertyChecks
import java.text.ParseException;

/**
 * @author Raymond Dodge
 * @version 2013 Jun 23
 */
class JSONArrayTest extends FunSpec
{
	describe ("A JSON Array") {
		describe ("when empty") {
			it ("claims to be empty") {
				assert( new JSONArray().isEmpty() )
			}
			it ("has a size of zero") {
				expectResult(0)( new JSONArray().size() )
			}
			it ("errors when getting") {
				intercept[IndexOutOfBoundsException]( new JSONArray().get(0) )
			}
			it ("has an unparsed value of `[]`") {
				expectResult("[]")( new JSONArray().getUnparsed() )
			}
		}
		
		describe ("with a single value added") {
			
			val value = 4
			def singleValueSeq = {
				val seq = new JSONArray();
				seq.add(value);
				seq
			}
			
			
			it ("claims to be not empty") {
				assert(! singleValueSeq.isEmpty() )
			}
			it ("has a size of one") {
				expectResult(1)( singleValueSeq.size() )
			}
			it ("returns the value when getting 0") {
				expectResult(value)( singleValueSeq.get(0) )
			}
			it ("errors when getting 1") {
				intercept[IndexOutOfBoundsException]( singleValueSeq.get(1) )
			}
			it ("has an unparsed value of `[4]`") {
				expectResult("[4]")( singleValueSeq.getUnparsed() )
			}
		}
		
		describe ("with many values added") {
			
			def size = 20
			def manyValueSeq = {
				val seq = new JSONArray();
				(0 until size).foreach{ seq.add(_) }
				seq
			}
			
			it ("claims to be not empty") {
				assert(! manyValueSeq.isEmpty() )
			}
			it ("has a size of size") {
				expectResult(size)( manyValueSeq.size() )
			}
			it ("returns the value when getting 0") {
				(0 until 20).foreach{ i => 
					expectResult(i)( manyValueSeq.get(i) )
				}
			}
		}
		
		it ("creates the correct values after insert") {
			
			def index = 10
			def value = true
			def manyValueSeq = {
				val seq = new JSONArray();
				(0 until 20).foreach{ seq.add(_) }
				seq.add(index, value)
				seq
			}
			
			(0 until index).foreach{ i => 
				expectResult(i)( manyValueSeq.get(i) )
			}
			expectResult(value)( manyValueSeq.get(index) )
			(index until 20).foreach{ i => 
				expectResult(i)( manyValueSeq.get(i+1) )
			}
			
			expectResult(
				"[0,1,2,3,4,5,6,7,8,9,true,10,11,"+
					"12,13,14,15,16,17,18,19]")(
				manyValueSeq.getUnparsed()
			)
			
		}
		
		it ("creates the correct values after remove") {
			
			def index = 10
			def manyValueSeq = {
				val seq = new JSONArray();
				(0 until 20).foreach{ seq.add(_) }
				seq.remove(index)
				seq
			}
			
			(0 until index).foreach{ i => 
				expectResult(i)( manyValueSeq.get(i) )
			}
			(index until 19).foreach{ i => 
				expectResult(i+1)( manyValueSeq.get(i) )
			}
			
			expectResult(
				"[0,1,2,3,4,5,6,7,8,9,11,"+
					"12,13,14,15,16,17,18,19]")(
				manyValueSeq.getUnparsed()
			)
			
		}
		
		it ("creates the correct values after set") {
			
			def index = 10
			def value = null
			def manyValueSeq = {
				val seq = new JSONArray();
				(0 until 20).foreach{ seq.add(_) }
				seq.set(index, value)
				seq
			}
			
			(0 until index).foreach{ i => 
				expectResult(i)( manyValueSeq.get(i) )
			}
			expectResult(value)( manyValueSeq.get(index) )
			((index+1) until 20).foreach{ i => 
				expectResult(i)( manyValueSeq.get(i) )
			}
			
			expectResult(
				"[0,1,2,3,4,5,6,7,8,9,null,11,"+
					"12,13,14,15,16,17,18,19]")(
				manyValueSeq.getUnparsed()
			)
			
		}
	}
}
