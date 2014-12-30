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
package com.rayrobdod.testing.javaScriptObjectNotation

import com.rayrobdod.javaScriptObjectNotation.parser._
import com.rayrobdod.javaScriptObjectNotation.parser.listeners._
import com.rayrobdod.javaScriptObjectNotation.parser.decoders.ToJavaCollectionJSONDecoder
import com.rayrobdod.javaScriptObjectNotation.JSONWriter
import org.scalatest.{FunSuite, FunSpec}
import org.scalatest.prop.PropertyChecks
import java.text.ParseException;
import scala.collection.JavaConversions.{mapAsJavaMap, seqAsJavaList}

/**
 * @author Raymond Dodge
 * @version 2013 Aug 03
 */
class JSONWriterTest extends FunSpec
{
	describe ("JSONWriter") {
		it ("should create a correct empty list") {
			assertResult("[]")(JSONWriter.encode(Seq.empty))
		}
		it ("should create a correct empty map") {
			assertResult("{}")(JSONWriter.encode(mapAsJavaMap(Map.empty[String,Object])))
		}
		it ("should create a correct one-element list") {
			assertResult("[ 5]")(JSONWriter.encode(Seq(5)))
		}
		it ("should create a correct one-pair map") {
			assertResult("{ \"1\":2}")(JSONWriter.encode(mapAsJavaMap(Map( "1" → 2))))
		}
		it ("should create a correct multi-element list") {
			assertResult("[ 1,2,3,4,5]")(JSONWriter.encode(Seq(1,2,3,4,5)))
		}
		it ("should create a correct multi-pair map") {
			val orig = (1 to 5).map{(a) => ((a.toString, a))}.toMap
			val string = JSONWriter.encode(mapAsJavaMap(orig));
			
			val l = new ToScalaCollection(new ToJavaCollectionJSONDecoder)
			JSONParser.parse(l, string)
			
			assertResult(orig)(l.resultMap)
		}
		it ("should recurse") {
			val orig = Seq(Seq(1, 2)).map{seqAsJavaList}
			val string = JSONWriter.encode(orig);
			
			assertResult("[ [ 1,2]]")(string)
		}
		it ("should do keywords") {
			val orig = Seq(true, false, null)
			val string = JSONWriter.encode(orig);
			
			assertResult("[ true,false,null]")(string)
		}
		it ("should do strings") {
			val orig = Seq("Hello")
			val string = JSONWriter.encode(orig);
			
			assertResult("[ \"Hello\"]")(string)
		}
	}
}
