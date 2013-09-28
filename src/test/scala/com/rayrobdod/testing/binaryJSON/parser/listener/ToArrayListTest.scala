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
