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

import scala.util.{Either, Left, Right}
import com.rayrobdod.json.builder.Builder
import com.rayrobdod.json.builder.MapBuilder
import com.rayrobdod.json.union.StringOrInt
import com.rayrobdod.json.union.JsonValue
import com.rayrobdod.json.union.ParserRetVal


/**
 * A streaming decoder for json data.
 * This parser is intended to be relatively strict.
 * 
 * @see [[http://json.org/]]
 * @version 3.0
 *
 * @constructor
 * Creates a JsonParser instance.
 */
final class JsonParser extends Parser[StringOrInt, JsonValue, Iterable[Char]] {
	private[this] type MBRST = MapBuilder.RecursiveSubjectType[StringOrInt, JsonValue]
	
	
	/**
	 * Decodes the input values to an object.
	 * @param builder the builder that this parser will use when constructing objects
	 * @param chars the serialized json object or array
	 * @return the parsed object
	 */
	def parse[A](builder:Builder[StringOrInt, JsonValue, A], chars:Iterable[Char]):ParserRetVal[A, Nothing] = {
		val endstate = chars.zipWithIndex.foldLeft[State[A]](new InitState(builder)){(state, charIndex) =>
			state.apply(charIndex._1, charIndex._2)
		}
		endstate match {
			case x:EndState[A] => ParserRetVal.Complex(x.result)
			case x:FailureState[A] => ParserRetVal.Failure(x.msg, x.off)
			case _ => ParserRetVal.Failure("incomplete object", chars.size)
		}
	}
	
	/**
	 * Decodes the input values to an object.
	 * @param chars the serialized json object or array
	 * @return the parsed object
	 */
	def parse[A](builder:Builder[StringOrInt, JsonValue, A], chars:java.io.Reader):ParserRetVal[A,JsonValue] = this.parse(builder, new Reader2Iterable(chars))
	
	
	/** The parser's state. To be placed inside a foldleft. */
	private[this] sealed trait State[A] {
		def apply(c:Char, index:Int):State[A]
	}
	
	/** State after finding the end of the outermost object */
	private[this] class EndState[A](val result:A) extends State[A] {
		def apply(c:Char, index:Int):State[A] = c match {
			case x if x.isWhitespace => this
			case _ => new FailureState("Stuff after end of object " + c, index)
		}
	}
	
	/** State before anything significant has been found */
	private[this] class InitState[A](builder:Builder[StringOrInt, JsonValue, A]) extends State[A] {
		def apply(c:Char, index:Int):State[A] = c match {
			case '\ufeff' => this // byte-order-mark
			case x if x.isWhitespace => this
			case '{'  => new ObjectKeyStartState(builder.init, builder, true, {a:A => new EndState(a)})
			case '['  => new ArrayValueStartState(builder.init, builder, {a:A => new EndState(a)})
			case _    => new FailureState("Expecting '{' or '['; found " + c, index)
		}
	}
	
	private[this] class ObjectKeyStartState[A, Curr](soFar:Curr, builder:Builder[StringOrInt, JsonValue, Curr], endObjectAllowed:Boolean, pop:Function1[Curr, State[A]]) extends State[A] {
		def apply(c:Char, index:Int):State[A] = c match {
			case x if x.isWhitespace => this
			case '"'  => new StringState("", {s:String => new ObjectKeyEndState(soFar, builder, s, pop)})
			case '}'  if endObjectAllowed => pop(soFar)
			case _    => new FailureState("Expecting start of key; found " + c, index)
		}
	}
	
	private[this] class ObjectKeyEndState[A, Curr](soFar:Curr, builder:Builder[StringOrInt, JsonValue, Curr], key:String, pop:Function1[Curr, State[A]]) extends State[A] {
		def apply(c:Char, index:Int):State[A] = c match {
			case x if x.isWhitespace => this
			case ':'  => new ObjectValueStartState[A, Curr](soFar, builder, key, pop)
			case _    => new FailureState("Expecting ':'; found " + c, index)
		}
	}
	
	private[this] class ObjectValueStartState[A, Curr](soFar:Curr, builder:Builder[StringOrInt, JsonValue, Curr], key:String, pop:Function1[Curr, State[A]]) extends State[A] {
		def apply(c:Char, index:Int):State[A] = c match {
			case x if x.isWhitespace => this
			case '"'  => new StringState("", {s:String =>
				builder.apply[JsonValue](soFar, key, JsonValue(s), new IdentityParser[JsonValue]()) match {
					case Right(x) => new ObjectValueEndState(x, builder, pop)
					case Left(x) => new FailureState(x._1, x._2 + index)
				}
			})
			case '['  => new ArrayValueStartState[A, MBRST](
				MapBuilder[StringOrInt, JsonValue].init,
				MapBuilder[StringOrInt, JsonValue],
				{a:MBRST => builder.apply(soFar, key, a, new RecusiveMapParser[StringOrInt, JsonValue]) match {
					case Right(x) => new ObjectValueEndState(x, builder, pop)
					case Left(x) => new FailureState(x._1, x._2 + index)
				}}
			)
			case '{'  => new ObjectKeyStartState[A, MBRST](
				MapBuilder[StringOrInt, JsonValue].init,
				MapBuilder[StringOrInt, JsonValue],
				true,
				{a:MBRST => builder.apply(soFar, key, a, new RecusiveMapParser[StringOrInt, JsonValue]) match {
					case Right(x) => new ObjectValueEndState(x, builder, pop)
					case Left(x) => new FailureState(x._1, x._2 + index)
				}}
			)
			case '-'  => new IntegerState("-", {s:Number =>
				builder.apply[JsonValue](soFar, key, JsonValue(s), new IdentityParser[JsonValue]()) match {
					case Right(x) => new ObjectValueEndState(x, builder, pop)
					case Left(x) => new FailureState(x._1, x._2 + index)
				}
			})
			case '.'  => {
				new FailureState("Numeric value may not begin with a '.'", index)
			}
			case x if ('0' <= x && x <= '9') => new IntegerState("" + x, {s:Number =>
				builder.apply[JsonValue](soFar, key, JsonValue(s), new IdentityParser[JsonValue]()) match {
					case Right(x) => new ObjectValueEndState(x, builder, pop)
					case Left(x) => new FailureState(x._1, x._2 + index)
				}
			})
			case x if ('a' <= x && x <= 'z') => new KeywordState("" + x, {s:JsonValue =>
				builder.apply[JsonValue](soFar, key, s, new IdentityParser[JsonValue]()) match {
					case Right(x) => new ObjectValueEndState(x, builder, pop)
					case Left(x) => new FailureState(x._1, x._2 + index)
				}
			})
			case _ => new FailureState("Expecting start of value; found " + c, index)
		}
	}
	
	private[this] class ObjectValueEndState[A, Curr](soFar:Curr, builder:Builder[StringOrInt, JsonValue, Curr], pop:Function1[Curr, State[A]]) extends State[A] {
		def apply(c:Char, charIndex:Int):State[A] = c match {
			case x if x.isWhitespace => this
			case ','  => new ObjectKeyStartState(soFar, builder, false, pop)
			case '}'  => pop(soFar) 
			case _    => new FailureState("Expecting ',' or '}'; found " + c, charIndex)
		}
	}
	
	
	private[this] class ArrayValueStartState[A, Curr](soFar:Curr, builder:Builder[StringOrInt, JsonValue, Curr], pop:Function1[Curr, State[A]], arrayIndex:Int = 0) extends State[A] {
		/** true iff the next character is allowed to end the array - i.e. be a ']' */
		private[this] val endObjectAllowed:Boolean = (arrayIndex == 0);
		
		def apply(c:Char, charIndex:Int):State[A] = c match {
			case x if x.isWhitespace => this
			case ']'  if endObjectAllowed => pop(soFar)
			case '"'  => new StringState("", {s:String =>
				builder.apply[JsonValue](soFar, arrayIndex, JsonValue(s), new IdentityParser[JsonValue]()) match {
					case Right(x) => new ArrayValueEndState(x, builder, arrayIndex, pop)
					case Left(x) => new FailureState(x._1, x._2 + charIndex)
				}
			})
			case '['  => new ArrayValueStartState[A, MBRST](
				MapBuilder[StringOrInt, JsonValue].init,
				MapBuilder[StringOrInt, JsonValue],
				{a:MBRST => builder.apply(soFar, arrayIndex, a, new RecusiveMapParser[StringOrInt, JsonValue]) match {
					case Right(x) => new ArrayValueEndState(x, builder, arrayIndex, pop)
					case Left(x) => new FailureState(x._1, x._2 + charIndex)
				}}
			)
			case '{'  => new ObjectKeyStartState[A, MBRST](
				MapBuilder[StringOrInt, JsonValue].init,
				MapBuilder[StringOrInt, JsonValue],
				true,
				{a:MBRST => builder.apply(soFar, arrayIndex, a, new RecusiveMapParser[StringOrInt, JsonValue]) match {
					case Right(x) => new ArrayValueEndState(x, builder, arrayIndex, pop)
					case Left(x) => new FailureState(x._1, x._2 + charIndex)
				}}
			)
			case '-'  => new IntegerState("-", {s:Number =>
				builder.apply[JsonValue](soFar, arrayIndex, JsonValue(s), new IdentityParser[JsonValue]()) match {
					case Right(x) => new ArrayValueEndState(x, builder, arrayIndex, pop)
					case Left(x) => new FailureState(x._1, x._2 + charIndex)
				}
			})
			case '.'  => {
				val msg = "Numeric value may not begin with a '.'"
				new FailureState(msg, charIndex);
			}
			case x if ('0' <= x && x <= '9') => new IntegerState("" + x, {s:Number =>
				builder.apply[JsonValue](soFar, arrayIndex, JsonValue(s), new IdentityParser[JsonValue]()) match {
					case Right(x) => new ArrayValueEndState(x, builder, arrayIndex, pop)
					case Left(x) => new FailureState(x._1, x._2 + charIndex)
				}
			})
			case x if ('a' <= x && x <= 'z') => new KeywordState("" + x, {s:JsonValue =>
				builder.apply[JsonValue](soFar, arrayIndex, s, new IdentityParser[JsonValue]()) match {
					case Right(x) => new ArrayValueEndState(x, builder, arrayIndex, pop)
					case Left(x) => new FailureState(x._1, x._2 + charIndex)
				}
			})
			case _ =>
				new FailureState("Expecting start of value; found " + c, charIndex)
		}
	}
	
	private[this] class ArrayValueEndState[A, Curr](soFar:Curr, builder:Builder[StringOrInt, JsonValue, Curr], arrayIndex:Int, pop:Function1[Curr, State[A]]) extends State[A] {
		def apply(c:Char, charIndex:Int):State[A] = c match {
			case x if x.isWhitespace => this
			case ','  => new ArrayValueStartState(soFar, builder, pop, arrayIndex + 1)
			case ']'  => pop(soFar)
			case _    => new FailureState("Expecting ',' or ']'; found " + c, charIndex)
		}
	}
	
	/** State while inside a string and no escaping is currently active */
	private[this] class StringState[A](str:String, pop:(String => State[A])) extends State[A] {
		def apply(c:Char, charIndex:Int):State[A] = {
			if (c < ' ') {
				new FailureState("Control chars not allowed in strings", charIndex)
			} else if (c == '\\') {
				new StringEscapeState(str, pop)
			} else if (c == '"') {
				pop(str)
			} else {
				new StringState(str + c, pop)
			}
		}
	}
	
	/** State while inside a string and the slash-escape just happened */
	private[this] class StringEscapeState[A](str:String, pop:(String => State[A])) extends State[A] {
		def apply(c:Char, charIndex:Int):State[A] = c match {
			case '"'  => new StringState(str + "\"", pop)
			case '\\' => new StringState(str + "\\", pop)
			case '/'  => new StringState(str + "/", pop)
			case 'b'  => new StringState(str + "\b", pop)
			case 'f'  => new StringState(str + "\f", pop)
			case 'n'  => new StringState(str + "\n", pop)
			case 'r'  => new StringState(str + "\r", pop)
			case 't'  => new StringState(str + "\t", pop)
			case 'u'  => new StringUnicodeEscapeState(str, pop)
			case _    => new FailureState("Unexpected escape code in string: " + c, charIndex)
		}
	}
	
	/** State while inside a string and processing a unicode-escape */
	private[this] class StringUnicodeEscapeState[A](str:String, pop:(String => State[A]), characters:Int = 0, value:Int = 0) extends State[A] {
		def apply(c:Char, charIndex:Int):State[A] = {
			if (('0' <= c && c <= '9') || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F')) {
				val cInt = java.lang.Integer.parseInt("" + c, 16)
				val newValue = value * 16 + cInt
				
				if (characters < 3) {
					new StringUnicodeEscapeState(str, pop, characters + 1, value * 16 + cInt)
				} else {
					new StringState(str + newValue.toChar, pop)
				}
			} else {
				new FailureState("non-hex character in unicode escape: " + c, charIndex)
			}
		}
	}
	
	/** State while inside an integer */
	private[this] class IntegerState[A](str:String, pop:(Number => State[A])) extends State[A] {
		def apply(c:Char, charIndex:Int):State[A] = {
			if (c == '}' || c == ']' || c == ',') {
				val rawNewValue = str.toString.trim
				try {
					val newValue:Number = scala.math.BigDecimal(rawNewValue)
					pop(newValue).apply(c, charIndex)
				} catch {
					case _:NumberFormatException => {
						new FailureState("Not a number: " + rawNewValue, charIndex)
					}
				}
			} else {
				new IntegerState(str + c, pop)
			}
		}
	}
	
	/** State while inside a keyword */
	private[this] class KeywordState[A](str:String, pop:(JsonValue => State[A])) extends State[A] {
		def apply(c:Char, charIndex:Int):State[A] = {
			if (c == '}' || c == ']' || c == ',') {
				val valueString = str.toString.trim
				val value = valueString match {
					case "true"  => Right(JsonValue(true))
					case "false" => Right(JsonValue(false))
					case "null"  => Right(JsonValue.JsonValueNull)
					case _ => Left("Unexpected keyword: " + valueString, charIndex)
				}
				value match {
					case Right(x) => pop(x).apply(c, charIndex)
					case Left(x) => new FailureState(x._1, x._2)
				}
			} else {
				new KeywordState(str + c, pop)
			}
		}
	}
	
	/** State after some failure has occured */
	private[this] class FailureState[A](val msg:String, val off:Int) extends State[A] {
		def apply(c:Char, charIndex:Int):State[A] = this
	}
}
