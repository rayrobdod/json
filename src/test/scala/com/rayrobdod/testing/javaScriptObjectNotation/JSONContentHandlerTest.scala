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
