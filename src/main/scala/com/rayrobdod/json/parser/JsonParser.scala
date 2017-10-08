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
import com.rayrobdod.json.builder.Builder
import com.rayrobdod.json.union.StringOrInt
import com.rayrobdod.json.union.JsonValue
import com.rayrobdod.json.union.ParserRetVal


/**
 * A streaming decoder for json data.
 * This parser is intended to be relatively strict.
 * 
 * @see [[http://json.org/]]
 * @version 4.0
 *
 * @constructor
 * Creates a JsonParser instance.
 */
final class JsonParser extends Parser[StringOrInt, JsonValue, JsonParser.Failures, CharacterIndex, CountingReader] {
	private[this] val identityParser = new IdentityParser[JsonValue]()
	import JsonParser.Failures._
	
	
	/**
	 * Decodes the input values to an object.
	 * @param builder the builder that this parser will use when constructing objects
	 * @param chars the serialized json object or array
	 * @return the parsed object
	 */
	def parse[A, BF](builder:Builder[StringOrInt, JsonValue, BF, A], chars:Iterable[Char]):ParserRetVal[A, Nothing, JsonParser.Failures, BF, CharacterIndex] = {
		this.parse(builder, new Iterator2Reader(chars.iterator))
	}
	
	
	def parse[A, BF](builder:Builder[StringOrInt, JsonValue, BF, A], chars:String):ParserRetVal[A, Nothing, JsonParser.Failures, BF, CharacterIndex] = {
		this.parse(builder, new java.io.StringReader(chars))
	}
	
	/**
	 * Decodes the input values to an object.
	 * @param builder the builder that this parser will use when constructing objects
	 * @param chars the serialized json object or array
	 * @return the parsed object
	 */
	def parse[A, BF](builder:Builder[StringOrInt, JsonValue, BF, A], chars:Reader):ParserRetVal[A, Nothing, JsonParser.Failures, BF, CharacterIndex] = {
		this.parse(builder, new CountingReader(chars))
	}
	
	def parse[A, BF](builder:Builder[StringOrInt, JsonValue, BF, A], chars:CountingReader):ParserRetVal[A, Nothing, JsonParser.Failures, BF, CharacterIndex] = {
		val startIndex = chars.index + 1
		val retVal = try {
			var c = chars.read()
			while (c.isWhitespace || c == '\ufeff') {c = chars.read()}
			c match {
				case '{' => parseObjectValue(builder)(builder.init, true)(chars)
				case '[' => parseArrayValue(builder)(builder.init, 0)(chars)
				case c => ParserRetVal.ParserFailure(UnexpectedChar(c, "'{' or '['", chars.index))
			}
		} catch {
			case ex:java.util.NoSuchElementException => ParserRetVal.ParserFailure(IncompleteObject)
			case ex:java.lang.StackOverflowError => ParserRetVal.ParserFailure(TooDeeplyNested)
		}
		retVal
			.parserFailure.map{_.increaseIndex(-startIndex)}
			.builderFailure.map{(b,x) => ((b, x - startIndex))}
			.complex.flatMap{builder.finish(chars.index - startIndex)}
		
	}
	
	/** Read an object value. Do not include the first `'{'` in `r` */
	@scala.annotation.tailrec
	private[this] def parseObjectValue[BF](builder:Builder[StringOrInt, JsonValue, BF, _])(soFar:builder.Middle, endObjectAllowed:Boolean)(r:CountingReader):ParserRetVal[builder.Middle, Nothing, JsonParser.Failures, BF, CharacterIndex] = {
		var c = r.read()
		while (c.isWhitespace) {c = r.read()}
		val keyOpt : JsonParser.MidObjectParseValue[String, Nothing] = c match {
			case '}'  if endObjectAllowed => {
				JsonParser.ReturnSuccess
			}
			case '"' => {
				parseString(r).fold({x => JsonParser.Failure(x)}, {x => JsonParser.WithAddedValue(x)})
			}
			case c => {
				JsonParser.Failure(UnexpectedChar(c, "start of object key", r.index))
			}
		}
		
		val newSoFarOpt : JsonParser.MidObjectParseValue[builder.Middle, BF] = keyOpt.flatMap{any =>
			var c = r.read()
			while (c.isWhitespace) {c = r.read()}
			c match {
				case ':' => JsonParser.WithAddedValue(any)
				case c   => JsonParser.Failure(UnexpectedChar(c, "':'", r.index))
			}
		}.flatMap{key =>
			var c = r.read()
			while (c.isWhitespace) {c = r.read()}
			val startCharIndex = r.index
			c match {
				case '"' => {
					parseString(r).fold(
						{err => JsonParser.Failure(err)},
						{str => builder.apply(soFar, key, JsonValue(str), identityParser, ()).fold(
							{succ => JsonParser.WithAddedValue(succ)},
							{x:Nothing => x},
							{x:Nothing => x},
							{(bf, ex:Unit) => JsonParser.BuilderFailure(bf, startCharIndex)}
						)}
					)
				}
				case '[' | '{' => {
					r.goBackOne()
					builder.apply(soFar, key, r, JsonParser.this, r.index).fold(
						{x => JsonParser.WithAddedValue(x)},
						{x:Nothing => x},
						{pf => JsonParser.Failure(pf.increaseIndex(startCharIndex))},
						{(bf, ex) => JsonParser.BuilderFailure(bf, ex + startCharIndex)}
					)
				}
				case '.' => {
					JsonParser.Failure(NumericValueStartedWithFullStop(r.index))
				}
				case x if ('0' <= x && x <= '9') || (x == '-') => {
					r.goBackOne()
					parseNumber(r).fold(
						{err => JsonParser.Failure(err)},
						{int => builder.apply(soFar, key, int, identityParser, ()).fold(
							{succ => JsonParser.WithAddedValue(succ)},
							{x:Nothing => x},
							{x:Nothing => x},
							{(bf, ex:Unit) => JsonParser.BuilderFailure(bf, startCharIndex)}
						)}
					)
				}
				case x if ('a' <= x && x <= 'z') => {
					r.goBackOne()
					parseKeyword(r).fold(
						{err => JsonParser.Failure(err)},
						{word => builder.apply(soFar, key, word, identityParser, ()).fold(
							{succ => JsonParser.WithAddedValue(succ)},
							{x:Nothing => x},
							{x:Nothing => x},
							{(bf, ex:Unit) => JsonParser.BuilderFailure(bf, startCharIndex)}
						)}
					)
				}
				case c => {
					JsonParser.Failure(UnexpectedChar(c, "start of value", r.index))
				}
			}
		}
		
		newSoFarOpt match {
			case JsonParser.ReturnSuccess => ParserRetVal.Complex(soFar)
			case JsonParser.Failure(err) => ParserRetVal.ParserFailure(err)
			case JsonParser.BuilderFailure(err, extra) => ParserRetVal.BuilderFailure(err, extra)
			case JsonParser.WithAddedValue(newSoFar) => {
				c = r.read()
				while (c.isWhitespace) {c = r.read()}
				c match {
					case ',' => parseObjectValue(builder)(newSoFar, false)(r)
					case '}' => ParserRetVal.Complex(newSoFar)
					case c   => ParserRetVal.ParserFailure(UnexpectedChar(c, "',' or '}'", r.index))
				}
			}
		}
	}
	
	/** Read an array value. Do not include the first `'['` in `r` */
	@scala.annotation.tailrec
	private[this] def parseArrayValue[BF](builder:Builder[StringOrInt, JsonValue, BF, _])(soFar:builder.Middle, arrayIndex:Int)(r:CountingReader):ParserRetVal[builder.Middle, Nothing, JsonParser.Failures, BF, CharacterIndex] = {
		/* true iff the next character is allowed to end the array - i.e. be a ']' */
		val endObjectAllowed:Boolean = (arrayIndex == 0);
		
		var c = r.read()
		while (c.isWhitespace) {c = r.read()}
		val startCharIndex = r.index
		val value : JsonParser.MidObjectParseValue[builder.Middle, BF] = c match {
			case ']'  if endObjectAllowed => {
				JsonParser.ReturnSuccess
			}
			case '"' => {
				parseString(r).fold(
					{err => JsonParser.Failure(err)},
					{str => builder.apply(soFar, arrayIndex, JsonValue(str), identityParser, ()).fold(
						{succ => JsonParser.WithAddedValue(succ)},
						{x:Nothing => x},
						{x:Nothing => x},
						{(bf, ex:Unit) => JsonParser.BuilderFailure(bf, startCharIndex)}
					)}
				)
			}
			case '[' | '{' => {
				r.goBackOne()
				builder.apply(soFar, arrayIndex, r, JsonParser.this, CharacterIndex.zero).fold(
					{x => JsonParser.WithAddedValue(x)},
					{x:Nothing => x},
					{pf => JsonParser.Failure(pf.increaseIndex(startCharIndex))},
					{(bf, ex) => JsonParser.BuilderFailure(bf, ex + startCharIndex)}
				)
			}
			case '.' => {
				JsonParser.Failure(NumericValueStartedWithFullStop(r.index))
			}
			case x if ('0' <= x && x <= '9') || (x == '-') => {
				r.goBackOne()
				parseNumber(r).fold(
					{err => JsonParser.Failure(err)},
					{int => builder.apply(soFar, arrayIndex, int, identityParser, ()).fold(
						{succ => JsonParser.WithAddedValue(succ)},
						{x:Nothing => x},
						{x:Nothing => x},
						{(bf, ex:Unit) => JsonParser.BuilderFailure(bf, startCharIndex)}
					)}
				)
			}
			case x if ('a' <= x && x <= 'z') => {
				r.goBackOne()
				parseKeyword(r).fold(
					{err => JsonParser.Failure(err)},
					{word => builder.apply(soFar, arrayIndex, word, identityParser, ()).fold(
						{succ => JsonParser.WithAddedValue(succ)},
						{x:Nothing => x},
						{x:Nothing => x},
						{(bf, ex:Unit) => JsonParser.BuilderFailure(bf, startCharIndex)}
					)}
				)
			}
			case c => {
				JsonParser.Failure(UnexpectedChar(c, "start of value", r.index))
			}
		}
		
		value match {
			case JsonParser.ReturnSuccess => ParserRetVal.Complex(soFar)
			case JsonParser.Failure(err) => ParserRetVal.ParserFailure(err)
			case JsonParser.BuilderFailure(err, extra) => ParserRetVal.BuilderFailure(err, extra)
			case JsonParser.WithAddedValue(newSoFar) => {
				c = r.read()
				while (c.isWhitespace) {c = r.read()}
				c match {
					case ',' => parseArrayValue(builder)(newSoFar, arrayIndex + 1)(r)
					case ']' => ParserRetVal.Complex(newSoFar)
					case c   => ParserRetVal.ParserFailure(UnexpectedChar(c, "',' or ']'", r.index))
				}
			}
		}
	}
	
	/** State while inside a string and no escaping is currently active. Do not include the first `'"'` in `r` */
	private[this] def parseString(r:CountingReader, soFar:String = ""):Either[JsonParser.Failures, String] = {
		val c = r.read()
		c match {
			case x if (x < ' ') => Left(ControlCharInString(x, r.index))
			case '\\' => parseEscape(r).right.flatMap{x => parseString(r, soFar + x)}
			case '"' => Right(soFar)
			case x => parseString(r, soFar + x)
		}
	}
	
	/** State while inside a string and the slash-escape just happened. Do not include the initial '\\' in `r` */
	private[this] def parseEscape(r:CountingReader):Either[JsonParser.Failures, Char] = {
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
			case c    => Left(IllegalEscape(c, r.index))
		}
	}
	
	/** State while inside a string and processing a unicode-escape. Do not include the initial '\\u' in `r` */
	private[this] def parseUnicodeEscape(r:CountingReader):Either[NotAUnicodeEscape, Char] = {
		val c1 = r.read()
		val c2 = r.read()
		val c3 = r.read()
		val c4 = r.read()
		try {
			Right(java.lang.Integer.parseInt(new String(Array(c1, c2, c3, c4)), 16).toChar)
		} catch {
			case ex:NumberFormatException =>
				Left(NotAUnicodeEscape(new String(Array(c1, c2, c3, c4)), r.index - 4))
		}
	}
	
	/** State while inside an integer. Include the first character of the number */
	private[this] def parseNumber(r:CountingReader):Either[NotANumber, JsonValue] = {
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
					Left(NotANumber(valueString, startCharIndex))
				// string terminated with a '.'
				} else if (""".+\.""".r.unapplySeq(valueString).isDefined) {
					Left(NotANumber(valueString, startCharIndex))
				// '.' not followed by a digit
				} else if (""".+\.[^\d].+""".r.unapplySeq(valueString).isDefined) {
					Left(NotANumber(valueString, startCharIndex))
				} else {
					Right(JsonValue(scala.math.BigDecimal(valueString)))
				}
			} catch {
				case ex:NumberFormatException =>
					Left(NotANumber(valueString, startCharIndex))
			}
		}
		value
	}
	
	/** State while inside a keyword. Include the first character of the keyword */
	private[this] def parseKeyword(r:CountingReader):Either[NotAKeyword, JsonValue] = {
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
			case _ => Left(NotAKeyword(valueString, startCharIndex))
		}
		value
	}
}

object JsonParser {
	
	/**
	 * Possible failures that can occur in a JsonParser
	 * @since 4.0
	 */
	sealed trait Failures {
		def increaseIndex(amount:CharacterIndex):Failures
	}
	/**
	 * Possible failures that can occur in a JsonParser
	 * @since 4.0
	 */
	object Failures {
		final case class NotAUnicodeEscape(hexPart:String, charIdx:CharacterIndex) extends Failures {
			def increaseIndex(amount:CharacterIndex):Failures = this.copy(charIdx = this.charIdx + amount)
		}
		final case class IllegalEscape(escChar:Char, charIdx:CharacterIndex) extends Failures {
			def increaseIndex(amount:CharacterIndex):Failures = this.copy(charIdx = this.charIdx + amount)
		}
		final case class ControlCharInString(char:Char, charIdx:CharacterIndex) extends Failures {
			def increaseIndex(amount:CharacterIndex):Failures = this.copy(charIdx = this.charIdx + amount)
		}
		final case class NotANumber(value:String, charIdx:CharacterIndex) extends Failures {
			def increaseIndex(amount:CharacterIndex):Failures = this.copy(charIdx = this.charIdx + amount)
		}
		final case class NotAKeyword(value:String, charIdx:CharacterIndex) extends Failures {
			def increaseIndex(amount:CharacterIndex):Failures = this.copy(charIdx = this.charIdx + amount)
		}
		final case class UnexpectedChar(was:Char, expecting:String, charIdx:CharacterIndex) extends Failures {
			def increaseIndex(amount:CharacterIndex):Failures = this.copy(charIdx = this.charIdx + amount)
		}
		final case class NumericValueStartedWithFullStop(charIdx:CharacterIndex) extends Failures {
			def increaseIndex(amount:CharacterIndex):Failures = this.copy(charIdx = this.charIdx + amount)
		}
		object TooDeeplyNested extends Failures {
			def increaseIndex(amount:CharacterIndex):Failures = this
		}
		object IncompleteObject extends Failures {
			def increaseIndex(amount:CharacterIndex):Failures = this
		}
	}
	
	
	private sealed trait MidObjectParseValue[+A, +BF] {
		/** Return `f(this.x)` if `this` is a [[WithAddedValue]]; else return `this` */
		def flatMap[AA, BBF >: BF](f:(A) => MidObjectParseValue[AA, BBF]):MidObjectParseValue[AA, BBF]
	}
	private final object ReturnSuccess extends MidObjectParseValue[Nothing, Nothing] {
		def flatMap[AA, BBF](f:(Nothing) => MidObjectParseValue[AA, BBF]) = ReturnSuccess
	}
	private final case class WithAddedValue[A](x:A) extends MidObjectParseValue[A, Nothing] {
		def flatMap[AA, BBF](f:(A) => MidObjectParseValue[AA, BBF]):MidObjectParseValue[AA, BBF] = f(x)
	}
	private final case class Failure(msg:Failures) extends MidObjectParseValue[Nothing, Nothing] {
		def flatMap[AA, BBF](f:(Nothing) => MidObjectParseValue[AA, BBF]) = this
	}
	private final case class BuilderFailure[BF](msg:BF, extra:CharacterIndex) extends MidObjectParseValue[Nothing, BF] {
		def flatMap[AA, BBF >: BF](f:(Nothing) => MidObjectParseValue[AA, BBF]) = this
	}
	
}
