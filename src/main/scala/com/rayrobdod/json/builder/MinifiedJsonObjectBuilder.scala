/*
	Copyright (c) 2015-2016, Raymond Dodge
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
import scala.util.{Either, Left, Right}
import java.nio.charset.StandardCharsets.UTF_8;
import java.nio.charset.Charset;
import com.rayrobdod.json.union.JsonValue
import com.rayrobdod.json.parser.Parser
import com.rayrobdod.json.parser.{MapParser, SeqParser}

/** A builder that will output an object as an json format string
 * 
 * @version next
 * @constructor
 * Creates a JsonObjectBuilder
 * 
 * @param charset The output will only contain characters that can be encoded using the specified charset.
 *           Any characters outside the charset will be u-escaped. Default is to keep all characters verbaitim
 */
@deprecated("use PrettyJsonBuilder(PrettyJsonBuilder.MinifiedPrettyParams) instead", "next")
final class MinifiedJsonObjectBuilder(charset:Charset = UTF_8) extends Builder[String, JsonValue, String] {
	import MinifiedJsonObjectBuilder._
	
	val init:String = "{}"
	
	/** @param folding a valid json object, with no characters trailing the final '}' */
	def apply[Input](folding:String, key:String, innerInput:Input, parser:Parser[String, JsonValue, Input]):Either[(String, Int), String] = {
		val jsonKey:String = strToJsonStr(key, charset)
		parser.parse(this, innerInput).fold({s => Right(s)}, {p => Right(serialize(p, charset))}, {(s,i) => Left((s,i))}).right.map{jsonObject =>
			val jsonKeyValuePair = jsonKey + ":" + jsonObject;
			
			if (folding == "{}") {
				"{" + jsonKeyValuePair + "}"
			} else {
				folding.init + "," + jsonKeyValuePair + "}"
			}	
		}
	}
}

/** A builder that will output an array as a json format string
 * 
 * @version next
 * @constructor
 * A builder that will create json format strings
 * 
 * @param charset The output will only contain characters that can be encoded using the specified charset.
 *           Any characters outside the charset will be u-escaped. Default is to keep all characters verbaitim
 */
@deprecated("use PrettyJsonBuilder(PrettyJsonBuilder.MinifiedPrettyParams) instead", "next")
final class MinifiedJsonArrayBuilder(charset:Charset = UTF_8) extends Builder[Any, JsonValue, String] {
	import MinifiedJsonObjectBuilder._
	
	val init:String = "[]"
	
	/** @param folding a valid json object, with no characters trailing the final '}' */
	def apply[Input](folding:String, key:Any, innerInput:Input, parser:Parser[Any, JsonValue, Input]):Either[(String, Int), String] = {
		parser.parse(this, innerInput).fold({s => Right(s)}, {p => Right(serialize(p, charset))}, {(s,i) => Left((s,i))}).right.map{jsonObject =>
			if (folding == "[]") {
				"[" + jsonObject + "]"
			} else {
				folding.init + "," + jsonObject + "]"
			}
		}
	}
}

/** methods for [[MinifiedJsonObjectBuilder]] and [[MinifiedJsonArrayBuilder]] */
private[builder] object MinifiedJsonObjectBuilder {
	import JsonValue._
	
	/** Encode a JsonValue as a serialized json value */
	private[builder] def serialize(value:JsonValue, charset:Charset):String = value match {
		case JsonValueNumber(x) => x.toString
		case JsonValueBoolean(x) => x.toString
		case JsonValueNull => "null"
		case JsonValueString(x) => strToJsonStr(x, charset)
	}
	
	/** Encode a string as a serialized json value */
	private[builder] def strToJsonStr(s:String, charset:Charset):String = "\"" + s.flatMap{_ match {
		case '"'  => "\\\""
		case '\\' => """\\"""
		case '\b' => "\\b"
		case '\f' => "\\f"
		case '\n' => "\\n"
		case '\r' => "\\r"
		case '\t' => "\\t"
		case x if (x < ' ') => toUnicodeEscape(x)
		case x if (! charset.newEncoder.canEncode(x)) => toUnicodeEscape(x)
		case x => Seq(x)
	}} + "\""
	
	@inline
	private def toUnicodeEscape(c:Char) = {
		"\\u" + ("0000" + c.intValue.toHexString).takeRight(4)
	}
}
