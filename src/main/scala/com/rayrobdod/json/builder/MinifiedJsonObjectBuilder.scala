/*
	Copyright (c) 2015, Raymond Dodge
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
package com.rayrobdod.json.builder;

import scala.collection.immutable.Seq;
import java.nio.charset.StandardCharsets.UTF_8;
import java.nio.charset.Charset;

/** A builder that will output a json format string
 * 
 * @constructor
 * A builder that will create json format strings
 * 
 * @param charset The output will only contain characters that can be encoded using the specified charset.
 *           Any characters outside the charset will be u-escaped. Default is to keep all characters verbaitim
 */
class MinifiedJsonObjectBuilder(charset:Charset = UTF_8) extends Builder[String] {
	
	val init:String = "{}"
	
	/** @param folding a valid json object, with no characters trailing the final '}' */
	def apply(folding:String, key:String, value:Any):String = {
		val jsonKey:String = strToJsonStr(key)
		
		val jsonObject:String = value match {
			case x:Number => x.toString
			case x:Boolean => x.toString
			case null => "null"
			case x:String => strToJsonStr(x)
			case x:Map[_,_] => new MapParser(this).parse(x.asInstanceOf[Map[Any, Any]])
		}
		
		val jsonKeyValuePair = jsonKey + ":" + jsonObject;
		
		if (folding == "{}") {
			"{" + jsonKeyValuePair + "}"
		} else {
			folding.init + "," + jsonKeyValuePair + "}"
		}
	}
	def childBuilder(key:String):Builder[_ <: Any] = new MapBuilder()
	val resultType:Class[String] = classOf[String]
	
	
	
	private def strToJsonStr(s:String):String = "\"" + s.flatMap{_ match {
		case '"'  => "\\\""
		case '\\' => """\\"""
		case '\b' => "\\b"
		case '\f' => "\\f"
		case '\n' => "\\n"
		case '\r' => "\\r"
		case '\t' => "\\t"
		case x => {
			if (x < ' ') {
				"\\u" + ("0000" + x.intValue.toHexString).takeRight(4)
			} else if (! charset.newEncoder().canEncode(x)) {
				"\\u" + ("0000" + x.intValue.toHexString).takeRight(4)
			} else {
				Seq(x)
			}
		}
	}} + "\""
	
	
	private class MapParser[A](topBuilder:Builder[A]) {
		def parse(vals:Map[Any, Any]):A = {
			vals.foldLeft[A](topBuilder.init){
				(state:A, keyValue:(Any, Any)) => topBuilder.apply(state, keyValue._1.toString, keyValue._2)
			}
		}
	}
}
