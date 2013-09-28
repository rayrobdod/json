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

import com.rayrobdod.javaScriptObjectNotation.JSONContentHandler
import java.io.ByteArrayInputStream;
import java.net.URL;
import org.scalatest.{FunSuite, FunSpec}
import org.scalatest.prop.PropertyChecks
import java.text.ParseException;

/**
 * @author Raymond Dodge
 * @version 2013 Sept 02
 */
class JSONContentHandlerTest extends FunSpec
{
	class FakeURLConnection(value:String) extends java.net.URLConnection(new URL("file", "", -1, "")) {
		
		override def connect() = {}
		override val getContentType = "application/json";
		override val getContentEncoding = "utf-8";
		
		override def getInputStream = {
			new ByteArrayInputStream(value.getBytes(this.getContentEncoding))
		}
		
	}

	val handler = new JSONContentHandler();
	val listClass = Array[java.lang.Class[_]](classOf[java.util.List[_]]);
	val mapClass  = Array[java.lang.Class[_]](classOf[java.util.Map[_,_]]);
	val bothClass = Array[java.lang.Class[_]](classOf[java.util.List[_]], classOf[java.util.Map[_,_]]);
	
	
	describe ("JSONContentHandler") {
		describe ("getContent(URLConnection)") {
			it ("should parse an empty array"){
				val thingToTest = "[]"
				val result = handler.getContent(new FakeURLConnection(thingToTest));
				
				assert( result.isInstanceOf[java.util.List[_]] )
				assert( result.asInstanceOf[java.util.List[_]].size == 0 )
			}
			it ("should parse an nonempty array"){
				val thingToTest = "[1,2,3]"
				val result = handler.getContent(new FakeURLConnection(thingToTest));
				
				assert( result.isInstanceOf[java.util.List[_]] )
				assert( result.asInstanceOf[java.util.List[_]].size == 3 )
				assert( result.asInstanceOf[java.util.List[_]].get(1) == 2 )
			}
			it ("should parse an empty object"){
				val thingToTest = "{}"
				val result = handler.getContent(new FakeURLConnection(thingToTest));
				
				assert( result.isInstanceOf[java.util.Map[_,_]] )
				assert( result.asInstanceOf[java.util.Map[_,_]].size == 0 )
			}
			it ("should parse an nonempty object"){
				val thingToTest = "{\"hello\":\"world\"}"
				val result = handler.getContent(new FakeURLConnection(thingToTest));
				
				assert( result.isInstanceOf[java.util.Map[_,_]] )
				assert( result.asInstanceOf[java.util.Map[_,_]].size == 1 )
				assert( result.asInstanceOf[java.util.Map[_,_]].get("hello").toString == "world" )
			}
		}
		
		describe ("getContent(URLConnection, listClass)") {
			it ("should parse an empty array"){
				val thingToTest = "[]"
				val result = handler.getContent(new FakeURLConnection(thingToTest), listClass);
				
				assert( result.isInstanceOf[java.util.List[_]] )
				assert( result.asInstanceOf[java.util.List[_]].size == 0 )
			}
			it ("should parse an nonempty array"){
				val thingToTest = "[1,2,3]"
				val result = handler.getContent(new FakeURLConnection(thingToTest), listClass);
				
				assert( result.isInstanceOf[java.util.List[_]] )
				assert( result.asInstanceOf[java.util.List[_]].size == 3 )
				assert( result.asInstanceOf[java.util.List[_]].get(1) == 2 )
			}
			it ("should not parse an empty object"){
				val thingToTest = "{}"
				val result = handler.getContent(new FakeURLConnection(thingToTest), listClass);
				
				assert( result == null )
			}
			it ("should not parse an nonempty object"){
				val thingToTest = "{\"hello\":\"world\"}"
				val result = handler.getContent(new FakeURLConnection(thingToTest), listClass);
				
				assert( result == null )
			}
		}
		
		describe ("getContent(URLConnection, mapClass)") {
			it ("should not parse an empty array"){
				val thingToTest = "[]"
				val result = handler.getContent(new FakeURLConnection(thingToTest), mapClass);
				
				assert( result == null )
			}
			it ("should not parse an nonempty array"){
				val thingToTest = "[1,2,3]"
				val result = handler.getContent(new FakeURLConnection(thingToTest), mapClass);
				
				assert( result == null )
			}
			it ("should parse an empty object"){
				val thingToTest = "{}"
				val result = handler.getContent(new FakeURLConnection(thingToTest), mapClass);
				
				assert( result.isInstanceOf[java.util.Map[_,_]] )
				assert( result.asInstanceOf[java.util.Map[_,_]].size == 0 )
			}
			it ("should parse an nonempty object"){
				val thingToTest = "{\"hello\":\"world\"}"
				val result = handler.getContent(new FakeURLConnection(thingToTest), mapClass);
				
				assert( result.isInstanceOf[java.util.Map[_,_]] )
				assert( result.asInstanceOf[java.util.Map[_,_]].size == 1 )
				assert( result.asInstanceOf[java.util.Map[_,_]].get("hello").toString == "world" )
			}
		}
		
		describe ("getContent(URLConnection, bothClass)") {
			it ("should parse an empty array"){
				val thingToTest = "[]"
				val result = handler.getContent(new FakeURLConnection(thingToTest), listClass);
				
				assert( result.isInstanceOf[java.util.List[_]] )
				assert( result.asInstanceOf[java.util.List[_]].size == 0 )
			}
			it ("should parse an nonempty array"){
				val thingToTest = "[1,2,3]"
				val result = handler.getContent(new FakeURLConnection(thingToTest), listClass);
				
				assert( result.isInstanceOf[java.util.List[_]] )
				assert( result.asInstanceOf[java.util.List[_]].size == 3 )
				assert( result.asInstanceOf[java.util.List[_]].get(1) == 2 )
			}
			it ("should parse an empty object"){
				val thingToTest = "{}"
				val result = handler.getContent(new FakeURLConnection(thingToTest), mapClass);
				
				assert( result.isInstanceOf[java.util.Map[_,_]] )
				assert( result.asInstanceOf[java.util.Map[_,_]].size == 0 )
			}
			it ("should parse an nonempty object"){
				val thingToTest = "{\"hello\":\"world\"}"
				val result = handler.getContent(new FakeURLConnection(thingToTest), mapClass);
				
				assert( result.isInstanceOf[java.util.Map[_,_]] )
				assert( result.asInstanceOf[java.util.Map[_,_]].size == 1 )
				assert( result.asInstanceOf[java.util.Map[_,_]].get("hello").toString == "world" )
			}
		}
	}
			
}
