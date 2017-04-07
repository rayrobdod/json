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
import java.nio.charset.StandardCharsets.UTF_8;
import java.nio.charset.Charset;
import com.rayrobdod.json.union.{StringOrInt, JsonValue}
import com.rayrobdod.json.union.ParserRetVal
import com.rayrobdod.json.union.ParserRetVal.{Complex, BuilderFailure}
import com.rayrobdod.json.parser.Parser


/**
 * A builder whose output is a json-formatted string.
 * 
 * @since 3.0
 * @version 4.0
 * @see [[http://json.org/]]
 * @constructor
 * Construct a PrettyJsonBuilder
 * @param params the amount whitespace to insert between tokens
 * @param level the indentation level of this builder instance
 * @param charset The output will only contain characters that can be encoded using the specified charset.
 *           Any characters outside the charset will be u-escaped. Default is to keep all characters that are allowed by Json.
 *           There may be problems if the charset does not include at least ASCII characters.
 */
final class PrettyJsonBuilder(params:PrettyJsonBuilder.PrettyParams, charset:Charset = UTF_8, level:Int = 0) extends Builder[StringOrInt, JsonValue, PrettyJsonBuilder.Failures, String] {
	import PrettyJsonBuilder.serialize
	import PrettyJsonBuilder.Failures._

	override type Middle = PrettyJsonBuilder.Middle

	/** A builder to be used when serializing any 'complex' children of the values this builder is dealing with */
	private[this] lazy val nextLevel = new PrettyJsonBuilder(params, charset, level + 1)
	
	val init:Middle = new Middle()
	
	def apply[Input, PF, BE](folding:Middle, key:StringOrInt, innerInput:Input, parser:Parser[StringOrInt, JsonValue, PF, BE, Input], extra:BE):ParserRetVal[Middle, Nothing, PF, PrettyJsonBuilder.Failures, BE] = {
		parser.parse(nextLevel, innerInput).primitive.map{p => serialize(p, charset)}.mergeToComplex.complex.flatMap{encodedValue =>
			if (init == folding) {
				key match {
					case StringOrInt.Right(0) => Complex(new Middle(false, 1, encodedValue :: Nil))
					case StringOrInt.Right(int) => BuilderFailure(ArrayKeyNotIncrementing(int, 0), extra)
					case StringOrInt.Left(str) => Complex(new Middle(true, 1, (serialize(str, charset) + params.colon(level) + encodedValue) :: Nil))
				}
			} else {
				key match {
					case StringOrInt.Left(str) => {
						if (folding.isObject) {
							Complex(folding.append(serialize(str, charset) + params.colon(level) + encodedValue))
						} else {
							BuilderFailure(KeyTypeChangedMidObject(key, KeyTypeChangedMidObject.ExpectingInt), extra)
						}
					}
					case StringOrInt.Right(int) => {
						if (folding.isObject) {
							BuilderFailure(KeyTypeChangedMidObject(key, KeyTypeChangedMidObject.ExpectingString), extra)
						} else if (int != folding.count) {
							BuilderFailure(ArrayKeyNotIncrementing(int, folding.count), extra)
						} else {
							Complex(folding.append(encodedValue))
						}
					}
				}
			}
		}
	}
	
	override def finish[BE](extra:BE)(folding:Middle):ParserRetVal.Complex[String] = ParserRetVal.Complex(folding.finish(params, level))
}

/**
 * PrettyParams and two implementations of it.
 * @since 3.0
 */
object PrettyJsonBuilder {
	
	/** Encode a JsonValue as a serialized json value */
	private def serialize(value:JsonValue, charset:Charset):String = value match {
		case JsonValue.JsonValueNumber(x) => x.toString
		case JsonValue.JsonValueBoolean(x) => x.toString
		case JsonValue.JsonValueNull => "null"
		case JsonValue.JsonValueString(x) => strToJsonStr(x, charset)
	}
	
	/** Encode a string as a serialized json value */
	private def strToJsonStr(s:String, charset:Charset):String = "\"" + s.flatMap{_ match {
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
	
	/** Convert a character into a string representing a unicode escape */
	@inline
	private[this] def toUnicodeEscape(c:Char) = {
		"\\u" + ("0000" + c.intValue.toHexString).takeRight(4)
	}
	
	/** PrettyJsonBuilder's Middle type */
	final case class Middle private[builder] (
		  val isObject:Boolean
		, val count:Int
		, private val parts:List[String]
	) {
		private[builder] def this() = this(false, 0, Nil)
		
		private[PrettyJsonBuilder] def append(x:String):Middle = new Middle(
			  this.isObject
			, this.count + 1
			, x :: this.parts
		)
		
		private[PrettyJsonBuilder] def finish(params:PrettyJsonBuilder.PrettyParams, level:Int):String = {
			val builder = new java.lang.StringBuilder()
			builder.append(if (isObject) {params.lbracket(level)} else {params.lbrace(level)})
			if (this.count >= 1) {
				parts.tail.reverse.foreach{x => builder.append(x).append(params.comma(level))}
				builder.append(parts.head)
			}
			builder.append(if (isObject) {params.rbracket(level)} else {params.rbrace(level)})
			builder.toString
		}
	}
	
	
	/**
	 * Shorthand for a PrettyJsonBuilder using a MinifiedPrettyParams
	 * @since 3.1
	 */
	def minified(charset:Charset = UTF_8):Builder[StringOrInt, JsonValue, PrettyJsonBuilder.Failures, String] = new PrettyJsonBuilder(MinifiedPrettyParams, charset)
	/**
	 * Shorthand for a PrettyJsonBuilder using an IndentPrettyParams using two spaces for the indent
	 * @since 3.1
	 */
	def space2(charset:Charset = UTF_8):Builder[StringOrInt, JsonValue, PrettyJsonBuilder.Failures, String] = new PrettyJsonBuilder(new IndentPrettyParams("  "), charset)
	/**
	 * Shorthand for a PrettyJsonBuilder using an IndentPrettyParams using four spaces for the indent
	 * @since 3.1
	 */
	def space4(charset:Charset = UTF_8):Builder[StringOrInt, JsonValue, PrettyJsonBuilder.Failures, String] = new PrettyJsonBuilder(new IndentPrettyParams("    "), charset)
	/**
	 * Shorthand for a PrettyJsonBuilder using an IndentPrettyParams using a tab for the indent
	 * @since 3.1
	 */
	def tabbed(charset:Charset = UTF_8):Builder[StringOrInt, JsonValue, PrettyJsonBuilder.Failures, String] = new PrettyJsonBuilder(new IndentPrettyParams(), charset)
	
	
	/**
	 * Possible failures that can occur in a PrettyJsonBuilder
	 * @since 4.0
	 */
	sealed trait Failures
	/**
	 * Possible failures that can occur in a PrettyJsonBuilder
	 * @since 4.0
	 */
	object Failures {
		final case class KeyTypeChangedMidObject(recieved:StringOrInt, expecting:KeyTypeChangedMidObject.ExpectingType) extends Failures
		object KeyTypeChangedMidObject {
			sealed trait ExpectingType
			object ExpectingInt extends ExpectingType
			object ExpectingString extends ExpectingType
		}
		final case class ArrayKeyNotIncrementing(recieved:Int, expecting:Int) extends Failures
	}
	
	
	/**
	 * The whitespace strings that will appear between significant portions of a serialized json file
	 * 
	 * @see [[http://argonaut.io/scaladocs/#argonaut.PrettyParams]]
	 * @since 3.0
	 * @define whitespace This value must contain only whitespace characters.
	 * @define levelParam @param level the indentation depth to create a string for.
	 */
	trait PrettyParams {
		/**
		 * The string that will appear on the left of a ':' mapping separator. $whitespace
		 * $levelParam
		 */
		def colonLeft(level:Int):String
		/**
		 * The string that will appear on the right of a ':' mapping separator. $whitespace
		 * $levelParam
		 */
		def colonRight(level:Int):String
		/**
		 * The string that will appear on the left of a ',' item separator. $whitespace
		 * $levelParam
		 */
		def commaLeft(level:Int):String
		/**
		 * The string that will appear on the right of a ',' item separator. $whitespace
		 * $levelParam
		 */
		def commaRight(level:Int):String
		/**
		 * The string that will appear on the left of a '{' object start. $whitespace
		 * $levelParam
		 */
		def lbraceLeft(level:Int):String
		/**
		 * The string that will appear on the right of a '{' object start. $whitespace
		 * $levelParam
		 */
		def lbraceRight(level:Int):String
		/**
		 * The string that will appear on the left of a '[' array start. $whitespace
		 * $levelParam
		 */
		def lbracketLeft(level:Int):String
		/**
		 * The string that will appear on the right of a '[' array start. $whitespace
		 * $levelParam
		 */
		def lbracketRight(level:Int):String
		/**
		 * The string that will appear on the left of a '}' object end. $whitespace
		 * $levelParam
		 */
		def rbraceLeft(level:Int):String
		/**
		 * The string that will appear on the right of a '}' object end. $whitespace
		 * $levelParam
		 */
		def rbraceRight(level:Int):String
		/**
		 * The string that will appear on the left of a ']' array end. $whitespace
		 * $levelParam
		 */
		def rbracketLeft(level:Int):String
		/**
		 * The string that will appear on the right of a ']' array end. $whitespace
		 * $levelParam
		 */
		def rbracketRight(level:Int):String
		
		
		/**
		 * The string that includes a ':' and the surrounding whitespace.
		 * $levelParam
		 */
		final def colon(level:Int):String = colonLeft(level) + ":" + colonRight(level)
		/**
		 * The string that includes a ',' and the surrounding whitespace.
		 * $levelParam
		 */
		final def comma(level:Int):String = commaLeft(level) + "," + commaRight(level)
		/**
		 * The string that includes a '[' and the surrounding whitespace.
		 * $levelParam
		 */
		final def lbrace(level:Int):String = lbraceLeft(level) + "[" + lbraceRight(level)
		/**
		 * The string that includes a '{' and the surrounding whitespace.
		 * $levelParam
		 */
		final def lbracket(level:Int):String = lbracketLeft(level) + "{" + lbracketRight(level)
		/**
		 * The string that includes a ']' and the surrounding whitespace.
		 * $levelParam
		 */
		final def rbrace(level:Int):String = rbraceLeft(level) + "]" + rbraceRight(level)
		/**
		 * The string that includes a '}' and the surrounding whitespace.
		 * $levelParam
		 */
		final def rbracket(level:Int):String = rbracketLeft(level) + "}" + rbracketRight(level)
	}
	
	/**
	 * A PrettyParams that will result in a minified json string. Every function returns the empty string.
	 * @since 3.0
	 * @example 
	 * {{{
	 * {"a":true,"b":[1,2,3]}
	 * }}}
	 */
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
	
	/**
	 * A PrettyParams for an indenting pattern with one space around each colon, commas at the end of the a line and brackets on their own line.
	 * @example
	 * {{{
	 * {
	 * 	"a" : true,
	 * 	"b" : [
	 * 		1,
	 * 		2,
	 * 		3
	 * 	]
	 * }
	 * }}}
	 * @since 3.0
	 * @define whitespace This value must contain only whitespace characters
	 * 
	 * @constructor
	 * @param tab the string to use as the indention. $whitespace
	 * @param newline the string to use as the newline character. $whitespace
	 */
	final class IndentPrettyParams(tab:String = "\t", newline:String = System.lineSeparator) extends PrettyParams {
		private[this] def indent(level:Int) = Seq.fill(level)(tab).mkString
		
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
