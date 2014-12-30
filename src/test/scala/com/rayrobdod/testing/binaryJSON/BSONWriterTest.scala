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
package com.rayrobdod.testing.binaryJSON

import com.rayrobdod.binaryJSON.parser._
import com.rayrobdod.binaryJSON.parser.listeners._
import com.rayrobdod.binaryJSON.parser.decoders.ToJavaCollectionBSONDecoder
import com.rayrobdod.testing.binaryJSON.parser.listeners.commonObjects
import com.rayrobdod.binaryJSON.{BSONWriter, NulCharacterInKeyException}
import org.scalatest.{FunSuite, FunSpec}
import org.scalatest.prop.PropertyChecks
import java.text.ParseException;
import scala.collection.JavaConversions.{mapAsJavaMap, seqAsJavaList}
import java.io.{DataInputStream, ByteArrayInputStream}

/**
 * @author Raymond Dodge
 * @version 2013 Aug 03
 */
class BSONWriterTest extends FunSpec
{
	describe ("BSONWriter") {
		it ("should create a correct empty list") {
			assertResult(
				Seq(5,0,0,0,0).map{_.byteValue}.toSeq
			)(
				BSONWriter.encode(Seq.empty).toSeq
			)
		}
		it ("should create a correct empty map") {
			assertResult(
				Seq(5,0,0,0,0).map{_.byteValue}.toSeq
			)(
				BSONWriter.encode(mapAsJavaMap(Map.empty[String,Object])).toSeq
			)
		}
		it ("should create a correct one-element list") {
			assertResult(
				Seq(12,0,0,0,0x10,'0',0,5,0,0,0,0).toSeq
			)(
				BSONWriter.encode(Seq(5)).toSeq
			)
		}
		it ("should create a correct one-pair map") {
			assertResult(
				Seq(12,0,0,0,0x10,'a',0,2,0,0,0,0).toSeq
			)(
				BSONWriter.encode(mapAsJavaMap(Map("a" → 2))).toSeq
			)
		}
		it ("should create a correct multi-element list") {
			assertResult(
				Seq(40,0,0,0,
					0x10,'0',0, 0,0,0,0,
					0x10,'1',0, 1,0,0,0,
					0x10,'2',0, 2,0,0,0,
					0x10,'3',0, 3,0,0,0,
					0x10,'4',0, 4,0,0,0,
				0).toSeq
			)(
				BSONWriter.encode(0 to 4).toSeq
			)
		}
		it ("should create a correct multi-pair map") {
			val orig = (1 to 5).map{(a) => ((a.toString, a))}.toMap
			val string = BSONWriter.encode(mapAsJavaMap(orig));
			
			val l = new ToHashMap(new ToJavaCollectionBSONDecoder)
			BSONParser.parse(l, commonObjects.DataInput(string))
			
			assertResult(mapAsJavaMap(orig))(l.getResult)
		}
		it ("should recurse") {
			assertResult(
				Seq(13,0,0,0,
					0x04,'0',0, 5,0,0,0,0,
				0).toSeq
			)(
				BSONWriter.encode(Seq(new java.util.ArrayList())).toSeq
			)
		}
		it ("should do keywords") {
			assertResult(
				Seq(16,0,0,0,
					0x08,'0',0,1,
					0x08,'1',0,0,
					0x0A,'2',0,
				0).toSeq
			)(
				BSONWriter.encode(Seq(true,false,null)).toSeq
			)
		}
		it ("should do strings") {
			assertResult(
				Seq(18,0,0,0,
					0x02,'0',0,6,0,0,0,'h','e','l','l','o',0,
				0).toSeq
			)(
				BSONWriter.encode(Seq("hello")).toSeq
			)
		}
		it ("should error when key contains NUL") {
			intercept[NulCharacterInKeyException] {
				BSONWriter.encode(Map("\0" -> "NUL"))
			}
			
			// log(BSONWriter.encode(Map("\0" -> "NUL")).toSeq)
		}
	}
}
