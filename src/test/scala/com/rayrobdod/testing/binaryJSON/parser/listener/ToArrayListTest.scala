package com.rayrobdod.testing.binaryJSON.parser.listeners

import com.rayrobdod.binaryJSON.parser._
import com.rayrobdod.binaryJSON.parser.listeners._
import com.rayrobdod.binaryJSON.parser.decoders.ToJavaCollectionBSONDecoder
import org.scalatest.{FunSpec}
import java.text.ParseException;
import java.util.Arrays.{asList => JList}

/**
 * @author Raymond Dodge
 * @version 2013 Aug 04
 */
class ToArrayListTest extends FunSpec
{
	describe ("ToArrayList - BSON") {
		it ("should accept an empty array") {
			val l = new ToArrayList(new ToJavaCollectionBSONDecoder)
			
			l.started();
			l.ended();
			l.getResult.isEmpty
		}
	}
	
	describe ("BSONParser.parse(ToArrayList)") {
		it ("should accept an empty array") {
			val l = new ToArrayList(new ToJavaCollectionBSONDecoder)
			BSONParser.parse(l, commonObjects.empty)
			l.getResult.isEmpty
		}
		
		it ("should accept a one-ToJavaCollectionBSONDecoder array") {
			val l = new ToArrayList(new ToJavaCollectionBSONDecoder)
			BSONParser.parse(l, commonObjects.helloWorld)
			expectResult("world")(l.getResult.get(0))
		}
		
		it ("should accept a many-element array") {
			val l = new ToArrayList(new ToJavaCollectionBSONDecoder)
			BSONParser.parse(l, commonObjects.countTo20)
			expectResult(1)(l.getResult.get(1))
			expectResult(2)(l.getResult.get(2))
			expectResult(3)(l.getResult.get(3))
			expectResult(4)(l.getResult.get(4))
			expectResult(5)(l.getResult.get(5))
		}
		
		it ("should accept NULs in values") {
			val l = new ToArrayList(new ToJavaCollectionBSONDecoder)
			BSONParser.parse(l, commonObjects.containsNul)
			expectResult("\0")(l.getResult.get(0))
		}
		
		it ("should recurse") {
			val l = new ToArrayList(new ToJavaCollectionBSONDecoder)
			BSONParser.parse(l, commonObjects.recursion)
			
			expectResult(JList(1,2))(l.getResult.get(0))
		}
		
	}
}
