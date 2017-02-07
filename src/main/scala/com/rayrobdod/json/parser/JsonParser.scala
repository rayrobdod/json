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
package com.rayrobdod.json.parser;

import java.io.Reader
import scala.util.{Either, Left, Right}
import scala.math.BigDecimal
import com.rayrobdod.json.builder.Builder
import com.rayrobdod.json.union.StringOrInt
import com.rayrobdod.json.union.JsonValue
import com.rayrobdod.json.union.ParserRetVal


/**
 * A streaming decoder for json data.
 * This parser is intended to be relatively strict.
 * 
 * @see [[http://json.org/]]
 * @version next
 *
 * @constructor
 * Creates a JsonParser instance.
 */
final class JsonParser extends Parser[StringOrInt, JsonValue, CountingReader] {
	private[this] val identityParser = new IdentityParser[JsonValue]()
	
	
	/**
	 * Decodes the input values to an object.
	 * @param builder the builder that this parser will use when constructing objects
	 * @param chars the serialized json object or array
	 * @return the parsed object
	 */
	def parse[A](builder:Builder[StringOrInt, JsonValue, A], chars:Iterable[Char]):ParserRetVal[A, Nothing] = {
		this.parse(builder, new Iterator2Reader(chars.iterator))
	}
	
	
	def parse[A](builder:Builder[StringOrInt, JsonValue, A], chars:String):ParserRetVal[A, Nothing] = {
		this.parse(builder, new java.io.StringReader(chars))
	}
	
	/**
	 * Decodes the input values to an object.
	 * @param builder the builder that this parser will use when constructing objects
	 * @param chars the serialized json object or array
	 * @return the parsed object
	 */
	def parse[A](builder:Builder[StringOrInt, JsonValue, A], chars:Reader):ParserRetVal[A, Nothing] = {
		this.parse(builder, new CountingReader(chars))
	}
	
	def parse[A](builder:Builder[StringOrInt, JsonValue, A], chars:CountingReader):ParserRetVal[A, Nothing] = {
		try {
			var c = chars.read()
			while (c.isWhitespace || c == '\ufeff') {c = chars.read()}
			ParserRetVal.eitherToComplex(c match {
				case '{' => parseObjectValue(builder.init, builder, true)(chars)
				case '[' => parseArrayValue(builder.init, builder)(chars)
				case c  => Left(("Expecting '{' or '['; found " + c, chars.index))
			})
		} catch {
			case ex:java.util.NoSuchElementException => ParserRetVal.Failure("incomplete object", chars.index)
			case ex:java.lang.StackOverflowError => ParserRetVal.Failure("too-deeply nested object", chars.index)
		}
	}
	
	/** Read an object value. Do not include the first `'{'` in `r` */
	@scala.annotation.tailrec
	private[this] def parseObjectValue[A](soFar:A, builder:Builder[StringOrInt, JsonValue, A], endObjectAllowed:Boolean)(r:CountingReader):Either[(String, Int), A] = {
		var c = r.read()
		while (c.isWhitespace) {c = r.read()}
		val keyOpt : JsonParser.MidObjectParseValue[String] = c match {
			case '}'  if endObjectAllowed => {
				JsonParser.ReturnSuccess
			}
			case '"' => {
				parseString(r).fold({x => JsonParser.Failure(x)}, {x => JsonParser.WithAddedValue(x)})
			}
			case c => {
				JsonParser.Failure(("Expecting start of object key; found " + c, r.index))
			}
		}
		
		val newSoFarOpt : JsonParser.MidObjectParseValue[A] = keyOpt.flatMap{any =>
			var c = r.read()
			while (c.isWhitespace) {c = r.read()}
			c match {
				case ':' => JsonParser.WithAddedValue(any)
				case c   => JsonParser.Failure(("Expecting ':'; found " + c, r.index))
			}
		}.flatMap{key =>
			var c = r.read()
			while (c.isWhitespace) {c = r.read()}
			val startCharIndex = r.index
			c match {
				case '"' => {
					parseString(r).right.flatMap{x =>
						builder.apply(soFar, key, JsonValue(x), identityParser).left.map{x => ((x._1, x._2 + startCharIndex))}
					}.fold({x => JsonParser.Failure(x)}, {x => JsonParser.WithAddedValue(x)})
				}
				case '[' | '{' => {
					r.goBackOne()
					builder.apply(soFar, key, r, JsonParser.this).fold({x => JsonParser.Failure(x)}, {x => JsonParser.WithAddedValue(x)})
				}
				case '.' => {
					JsonParser.Failure(("Numeric value may not begin with a '.'", r.index))
				}
				case x if ('0' <= x && x <= '9') || (x == '-') => {
					r.goBackOne()
					parseNumber(r).right.flatMap{x =>
						builder.apply(soFar, key, x, identityParser).left.map{x => ((x._1, x._2 + startCharIndex))}
					}.fold({x => JsonParser.Failure(x)}, {x => JsonParser.WithAddedValue(x)})
				}
				case x if ('a' <= x && x <= 'z') => {
					r.goBackOne()
					parseKeyword(r).right.flatMap{x =>
						builder.apply(soFar, key, x, identityParser).left.map{x => ((x._1, x._2 + startCharIndex))}
					}.fold({x => JsonParser.Failure(x)}, {x => JsonParser.WithAddedValue(x)})
				}
				case c => {
					JsonParser.Failure("Expecting start of value; found " + c, r.index)
				}
			}
		}
		
		newSoFarOpt match {
			case JsonParser.ReturnSuccess => Right(soFar)
			case JsonParser.Failure(errMsg) => Left(errMsg)
			case JsonParser.WithAddedValue(newSoFar) => {
				c = r.read()
				while (c.isWhitespace) {c = r.read()}
				c match {
					case ',' => parseObjectValue(newSoFar, builder, false)(r)
					case '}' => Right(newSoFar)
					case c   => Left(("Expecting ',' or ']'; found " + c, r.index))
				}
			}
		}
	}
	
	/** Read an array value. Do not include the first `'['` in `r` */
	@scala.annotation.tailrec
	private[this] def parseArrayValue[A](soFar:A, builder:Builder[StringOrInt, JsonValue, A], arrayIndex:Int = 0)(r:CountingReader):Either[(String, Int), A] = {
		/** true iff the next character is allowed to end the array - i.e. be a ']' */
		val endObjectAllowed:Boolean = (arrayIndex == 0);
		
		var c = r.read()
		while (c.isWhitespace) {c = r.read()}
		val startCharIndex = r.index
		val value : JsonParser.MidObjectParseValue[A] = c match {
			case ']'  if endObjectAllowed => {
				JsonParser.ReturnSuccess
			}
			case '"' => {
				parseString(r).right.flatMap{x =>
					builder.apply(soFar, arrayIndex, JsonValue(x), identityParser).left.map{x => ((x._1, x._2 + startCharIndex))}
				}.fold({x => JsonParser.Failure(x)}, {x => JsonParser.WithAddedValue(x)})
			}
			case '[' | '{' => {
				r.goBackOne()
				builder.apply(soFar, arrayIndex, r, JsonParser.this).fold({x => JsonParser.Failure(x)}, {x => JsonParser.WithAddedValue(x)})
			}
			case '.' => {
				JsonParser.Failure(("Numeric value may not begin with a '.'", r.index))
			}
			case x if ('0' <= x && x <= '9') || (x == '-') => {
				r.goBackOne()
				parseNumber(r).right.flatMap{x =>
					builder.apply(soFar, arrayIndex, x, identityParser).left.map{x => ((x._1, x._2 + startCharIndex))}
				}.fold({x => JsonParser.Failure(x)}, {x => JsonParser.WithAddedValue(x)})
			}
			case x if ('a' <= x && x <= 'z') => {
				r.goBackOne()
				parseKeyword(r).right.flatMap{x =>
					builder.apply(soFar, arrayIndex, x, identityParser).left.map{x => ((x._1, x._2 + startCharIndex))}
				}.fold({x => JsonParser.Failure(x)}, {x => JsonParser.WithAddedValue(x)})
			}
			case c => {
				JsonParser.Failure("Expecting start of value; found " + c, r.index)
			}
		}
		
		value match {
			case JsonParser.ReturnSuccess => Right(soFar)
			case JsonParser.Failure(errMsg) => Left(errMsg)
			case JsonParser.WithAddedValue(newSoFar) => {
				c = r.read()
				while (c.isWhitespace) {c = r.read()}
				c match {
					case ',' => parseArrayValue(newSoFar, builder, arrayIndex + 1)(r)
					case ']' => Right(newSoFar)
					case c   => Left(("Expecting ',' or ']'; found " + c, r.index))
				}
			}
		}
	}
	
	/** State while inside a string and no escaping is currently active. Do not include the first `'"'` in `r` */
	private[this] def parseString(r:CountingReader, soFar:String = ""):Either[(String, Int), String] = {
		val c = r.read()
		c match {
			case x if (x < ' ') => Left("Control chars not allowed in strings", r.index)
			case '\\' => parseEscape(r).right.flatMap{x => parseString(r, soFar + x)}
			case '"' => Right(soFar)
			case x => parseString(r, soFar + x)
		}
	}
	
	/** State while inside a string and the slash-escape just happened. Do not include the initial '\\' in `r` */
	private[this] def parseEscape(r:CountingReader):Either[(String, Int), Char] = {
		r.read() match {
			case '"'  => Right('\"')
			case '\\' => Right('\\')
			case '/'  => Right('/')
			case 'b'  => Right('\b')
			case 'f'  => Right('\f')
			case 'n'  => Right('\n')
			case 'r'  => Right('\r')
			case 't'  => Right('\t')
			case 'u'  => parseUnicodeEscape(r)
			case c    => Left(("Unexpected escape code in string: " + c, r.index))
		}
	}
	
	/** State while inside a string and processing a unicode-escape. Do not include the initial '\\u' in `r` */
	private[this] def parseUnicodeEscape(r:CountingReader):Either[(String, Int), Char] = {
		val c1 = r.read()
		val c2 = r.read()
		val c3 = r.read()
		val c4 = r.read()
		try {
			Right(java.lang.Integer.parseInt(new String(Array(c1, c2, c3, c4)), 16).toChar)
		} catch {
			case ex:NumberFormatException =>
				Left(("Non-hex character in unicode escape: " + c1 + c2 + c3 + c4, r.index - 4))
		}
	}
	
	/** State while inside an integer. Include the first character of the number */
	private[this] def parseNumber(r:CountingReader):Either[(String, Int), JsonValue] = {
		val builder = new java.lang.StringBuilder;
		var c:Char = r.read()
		val startCharIndex = r.index
		while (!(c == '}' || c == ']' || c == ',')) {
			builder.append(c)
			c = r.read()
		}
		r.goBackOne()
		
		val valueString = builder.toString.trim
		val value = {
			try {
				// Leading zero in 
				if ("""-?0\d.+""".r.unapplySeq(valueString).isDefined) {
					Left(("Not a number: " + valueString, startCharIndex))
				// string terminated with a '.'
				} else if (""".+\.""".r.unapplySeq(valueString).isDefined) {
					Left(("Not a number: " + valueString, startCharIndex))
				// '.' not followed by a digit
				} else if (""".+\.[^\d].+""".r.unapplySeq(valueString).isDefined) {
					Left(("Not a number: " + valueString, startCharIndex))
				} else {
					Right(JsonValue(scala.math.BigDecimal(valueString)))
				}
			} catch {
				case ex:NumberFormatException =>
					Left(("Not a number: " + valueString, startCharIndex))
			}
		}
		value
	}
	
	/** State while inside a keyword. Include the first character of the keyword */
	private[this] def parseKeyword(r:CountingReader):Either[(String, Int), JsonValue] = {
		val builder = new java.lang.StringBuilder;
		var c:Char = r.read()
		val startCharIndex = r.index
		while (!(c == '}' || c == ']' || c == ',')) {
			builder.append(c)
			c = r.read()
		}
		r.goBackOne()
		
		val valueString = builder.toString.trim
		val value = valueString match {
			case "true"  => Right(JsonValue(true))
			case "false" => Right(JsonValue(false))
			case "null"  => Right(JsonValue.JsonValueNull)
			case _ => Left("Unexpected keyword: " + valueString, startCharIndex)
		}
		value
	}
}

private object JsonParser {
	
	private sealed trait MidObjectParseValue[+A] {
		/** Return `f(this.x)` if `this` is a [[WithAddedValue]]; else return `this` */
		def flatMap[B](f:(A) => MidObjectParseValue[B]):MidObjectParseValue[B]
	}
	private final object ReturnSuccess extends MidObjectParseValue[Nothing] {
		def flatMap[B](f:(Nothing) => MidObjectParseValue[B]) = ReturnSuccess
	}
	private final case class WithAddedValue[A](x:A) extends MidObjectParseValue[A] {
		def flatMap[B](f:(A) => MidObjectParseValue[B]):MidObjectParseValue[B] = f(x)
	}
	private final case class Failure(msg:(String, Int)) extends MidObjectParseValue[Nothing] {
		def flatMap[B](f:(Nothing) => MidObjectParseValue[B]) = this
	}
}
