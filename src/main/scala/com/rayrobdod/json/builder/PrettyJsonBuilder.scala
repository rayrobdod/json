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
import com.rayrobdod.json.union.{StringOrInt, JsonValue}
import com.rayrobdod.json.parser.{Parser, MapParser, SeqParser}


/**
 * Will build a serialized-json representation of an object
 * 
 * @param params the pretty-printing parameters.
 * @param level the indentation level of this builder instance
 * @param charset The output will only contain characters that can be encoded using the specified charset.
 *           Any characters outside the charset will be u-escaped. Default is to keep all characters verbaitim
 * @see http://argonaut.io/scaladocs/#argonaut.PrettyParams the only decent idea in argonaut
 */
final class PrettyJsonBuilder(params:PrettyJsonBuilder.PrettyParams, charset:Charset = UTF_8, level:Int = 0) extends Builder[StringOrInt, JsonValue, String] {
	import MinifiedJsonObjectBuilder.serialize

	private[this] lazy val nextLevel = new PrettyJsonBuilder(params, charset, level + 1)
	
	val init:String = params.lbrace(level) + params.rbrace(level)
	
	def apply[Input](key:StringOrInt):Function3[String, Input, Parser[StringOrInt, JsonValue, Input], String] = {(folding, innerInput, parser) =>
		val value = parser.parse(nextLevel, innerInput)
		val encodedValue = value match {
			case Left(x) => x
			case Right(x) => serialize(x, charset)
		}
			
		
		if (init == folding) {
			key match {
				case StringOrInt.Right(0) => params.lbrace(level) + encodedValue + params.rbrace(level)
				case StringOrInt.Right(int) => throw new IllegalArgumentException(s"Key: $int") // params.lbracket(level) + serialze(int.toString, charset) + params.colon(level) + encodedValue + params.rbrace(level)
				case StringOrInt.Left(str) => params.lbracket(level) + serialize(str, charset) + params.colon(level) + encodedValue + params.rbracket(level)
			}
		} else {
			val bracket:Boolean = folding.take(params.lbracket(level).length) == params.lbracket(level)
			val brace:Boolean = folding.take(params.lbrace(level).length) == params.lbrace(level)
			val keptPart:String = {
				if (bracket) {folding.dropRight(params.rbracket(level).length)}
				else if (brace) {folding.dropRight(params.rbrace(level).length)}
				else {throw new IllegalArgumentException("folding is wrong")}
			}
			
			key match {
				case StringOrInt.Right(int) if brace => {
					keptPart + params.comma(level) + encodedValue + params.rbrace(level)
				}
				case StringOrInt.Left(str) if bracket => {
					keptPart + params.comma(level) + serialize(str, charset) + params.colon(level) + encodedValue + params.rbracket(level)
				}
				case _ => throw new IllegalArgumentException("Key type changed mid-object")
			}
		}
	}
	
}

object PrettyJsonBuilder {
	/** The whitespace strings that will appear between significant portions of a serialized json file */
	trait PrettyParams {
		/** The string that will appear on the left of a ':' mapping separator */
		def colonLeft(level:Int):String
		/** The string that will appear on the right of a ':' mapping separator */
		def colonRight(level:Int):String
		/** The string that will appear on the left of a ',' item separator */
		def commaLeft(level:Int):String
		/** The string that will appear on the right of a ',' item separator */
		def commaRight(level:Int):String
		def lbraceLeft(level:Int):String
		def lbraceRight(level:Int):String
		def lbracketLeft(level:Int):String
		def lbracketRight(level:Int):String
		def rbraceLeft(level:Int):String
		def rbraceRight(level:Int):String
		def rbracketLeft(level:Int):String
		def rbracketRight(level:Int):String
		
		
		final def colon(level:Int):String = colonLeft(level) + ":" + colonRight(level)
		final def comma(level:Int):String = commaLeft(level) + "," + commaRight(level)
		final def lbrace(level:Int):String = lbraceLeft(level) + "[" + lbraceRight(level)
		final def lbracket(level:Int):String = lbracketLeft(level) + "{" + lbracketRight(level)
		final def rbrace(level:Int):String = rbraceLeft(level) + "]" + rbraceRight(level)
		final def rbracket(level:Int):String = rbracketLeft(level) + "}" + rbracketRight(level)
	}
	
	/** A PrettyParams that will result in a minified json string */
	object MinifiedPrettyParams extends PrettyParams {
		def colonLeft(level:Int):String = ""
		def colonRight(level:Int):String = ""
		def commaLeft(level:Int):String = ""
		def commaRight(level:Int):String = ""
		def lbraceLeft(level:Int):String = ""
		def lbraceRight(level:Int):String = ""
		def lbracketLeft(level:Int):String = ""
		def lbracketRight(level:Int):String = ""
		def rbraceLeft(level:Int):String = ""
		def rbraceRight(level:Int):String = ""
		def rbracketLeft(level:Int):String = ""
		def rbracketRight(level:Int):String = ""
	}
	class IndentPrettyParams(tab:String = "\t", newline:String = System.lineSeparator) extends PrettyParams {
		private def indent(level:Int) = Seq.fill(level)(tab).mkString
		
		def colonLeft(level:Int):String = " "
		def colonRight(level:Int):String = " "
		def commaLeft(level:Int):String = ""
		def commaRight(level:Int):String = newline + indent(level + 1)
		def lbraceLeft(level:Int):String = ""
		def lbraceRight(level:Int):String = newline + indent(level + 1)
		def lbracketLeft(level:Int):String = ""
		def lbracketRight(level:Int):String = newline + indent(level + 1)
		def rbraceLeft(level:Int):String = newline + indent(level)
		def rbraceRight(level:Int):String = ""
		def rbracketLeft(level:Int):String = newline + indent(level)
		def rbracketRight(level:Int):String = ""
	}
}
