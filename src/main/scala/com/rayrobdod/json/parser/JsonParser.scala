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

import java.text.ParseException
import scala.collection.immutable.{Seq, Map, Stack}
import scala.util.{Try, Success, Failure}
import com.rayrobdod.json.builder._
import com.rayrobdod.json.union.StringOrInt
import com.rayrobdod.json.union.JsonValue


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
final class JsonParser extends Parser[StringOrInt, JsonValue, Iterable[Char]] {
	
	/**
	 * Decodes the input values to an object.
	 * @param builder the builder that this parser will use when constructing objects
	 * @param chars the serialized json object or array
	 * @return the parsed object
	 */
	def parse[A](builder:Builder[StringOrInt, JsonValue, A], chars:Iterable[Char]):Try[Either[A,JsonValue]] = {
		val endstate = chars.zipWithIndex.foldLeft[State[A]](new InitState(builder)){(state, charIndex) =>
			state.apply(charIndex._1, charIndex._2)
		}
		endstate match {
			case x:EndState[A] => Try(Left(x.result))
			case x:FailureState[A] => Failure(x.ex)
			case _ => Failure(new ParseException("incomplete object", chars.size))
		}
	}
	
	/**
	 * Decodes the input values to an object.
	 * @param chars the serialized json object or array
	 * @return the parsed object
	 */
	def parse[A](builder:Builder[StringOrInt, JsonValue, A], chars:java.io.Reader):Try[Either[A,JsonValue]] = this.parse(builder, new Reader2Iterable(chars))
	
	
	/** The parser's state. To be placed inside a foldleft. */
	private[this] sealed trait State[A] {
		def apply(c:Char, index:Int):State[A]
	}
	
	private[this] class EndState[A](val result:A) extends State[A] {
		def apply(c:Char, index:Int):State[A] = c match {
			case x if x.isWhitespace => this
			case _ => new FailureState(new ParseException("Stuff after end of object " + c, index))
		}
	}
	
	private[this] class InitState[A](builder:Builder[StringOrInt, JsonValue, A]) extends State[A] {
		def apply(c:Char, index:Int):State[A] = c match {
			case '\ufeff' => this // byte-order-mark
			case x if x.isWhitespace => this
			case '{'  => new ObjectKeyStartState(builder.init, builder, true)
			case '['  => new ArrayValueStartState(builder.init, builder)
			case _    => new FailureState(new ParseException("Expecting '{' or '['; found " + c, index))
		}
	}
	
	private[this] class ObjectKeyStartState[A](soFar:A, builder:Builder[StringOrInt, JsonValue, A], endObjectAllowed:Boolean) extends State[A] {
		def apply(c:Char, index:Int):State[A] = c match {
			case x if x.isWhitespace => this
			case '"'  => new StringState("", {s:String => new ObjectKeyEndState(soFar, builder, s)})
			case '}'  if endObjectAllowed => new EndState(soFar)
			case _    => new FailureState(new ParseException("Expecting start of key; found " + c, index))
		}
	}
	
	private[this] class ObjectKeyEndState[A](soFar:A, builder:Builder[StringOrInt, JsonValue, A], key:String) extends State[A] {
		def apply(c:Char, index:Int):State[A] = c match {
			case x if x.isWhitespace => this
			case ':'  => new ObjectValueStartState[A](soFar, builder, key)
			case _    => new FailureState(new ParseException("Expecting ':'; found " + c, index))
		}
	}
	
	private[this] class ObjectValueStartState[A](soFar:A, builder:Builder[StringOrInt, JsonValue, A], key:String) extends State[A] {
		def apply(c:Char, index:Int):State[A] = c match {
			case x if x.isWhitespace => this
			case '"'  => new StringState("", {s:String =>
				builder.apply[JsonValue](soFar, key, JsonValue(s), new IdentityParser[StringOrInt, JsonValue]()) match {
					case Success(x) => new ObjectValueEndState(x, builder)
					case Failure(x) => new FailureState("", index, x)
				}
			})
			case '['  => new InnerObjectState("[", {s:String => 
				builder.apply[Iterable[Char]](soFar, key, s, JsonParser.this) match {
					case Success(x) => new ObjectValueEndState(x, builder)
					case Failure(x) => new FailureState("", index, x)
				}
			})
			case '{'  => new InnerObjectState("{", {s:String => 
				builder.apply[Iterable[Char]](soFar, key, s, JsonParser.this) match {
					case Success(x) => new ObjectValueEndState(x, builder)
					case Failure(x) => new FailureState("", index, x)
				}
			})
			case '-'  => new IntegerState("-", {s:Number =>
				builder.apply[JsonValue](soFar, key, JsonValue(s), new IdentityParser[StringOrInt, JsonValue]()) match {
					case Success(x) => new ObjectValueEndState(x, builder)
					case Failure(x) => new FailureState("", index, x)
				}
			})
			case '.'  => {
				val msg = "Numeric value may not begin with a '.'"
				new FailureState(msg, index, new NumberFormatException(msg))
			}
			case x if ('0' <= x && x <= '9') => new IntegerState("" + x, {s:Number =>
				builder.apply[JsonValue](soFar, key, JsonValue(s), new IdentityParser[StringOrInt, JsonValue]()) match {
					case Success(x) => new ObjectValueEndState(x, builder)
					case Failure(x) => new FailureState("", index, x)
				}
			})
			case x if ('a' <= x && x <= 'z') => new KeywordState("" + x, {s:JsonValue =>
				builder.apply[JsonValue](soFar, key, s, new IdentityParser[StringOrInt, JsonValue]()) match {
					case Success(x) => new ObjectValueEndState(x, builder)
					case Failure(x) => new FailureState("", index, x)
				}
			})
			case _ => new FailureState(new ParseException("Expecting start of value; found " + c, index))
		}
	}
	
	private[this] class ObjectValueEndState[A](soFar:A, builder:Builder[StringOrInt, JsonValue, A]) extends State[A] {
		def apply(c:Char, charIndex:Int):State[A] = c match {
			case x if x.isWhitespace => this
			case ','  => new ObjectKeyStartState(soFar, builder, false)
			case '}'  => new EndState(soFar) 
			case _    => new FailureState(new ParseException("Expecting ',' or '}'; found " + c, charIndex))
		}
	}
	
	
	private[this] class ArrayValueStartState[A](soFar:A, builder:Builder[StringOrInt, JsonValue, A], arrayIndex:Int = 0) extends State[A] {
		/** true iff the next character is allowed to end the array - i.e. be a ']' */
		private[this] val endObjectAllowed:Boolean = (arrayIndex == 0);
		
		def apply(c:Char, charIndex:Int):State[A] = c match {
			case x if x.isWhitespace => this
			case ']'  if endObjectAllowed => new EndState(soFar)
			case '"'  => new StringState("", {s:String =>
				builder.apply[JsonValue](soFar, arrayIndex, JsonValue(s), new IdentityParser[StringOrInt, JsonValue]()) match {
					case Success(x) => new ArrayValueEndState(x, builder, arrayIndex)
					case Failure(x) => new FailureState("", charIndex, x)
				}
			})
			case '['  => new InnerObjectState("[", {s:String => 
				builder.apply[Iterable[Char]](soFar, arrayIndex, s, JsonParser.this) match {
					case Success(x) => new ArrayValueEndState(x, builder, arrayIndex)
					case Failure(x) => new FailureState("", charIndex, x)
				}
			})
			case '{'  => new InnerObjectState("{", {s:String => 
				builder.apply[Iterable[Char]](soFar, arrayIndex, s, JsonParser.this) match {
					case Success(x) => new ArrayValueEndState(x, builder, arrayIndex)
					case Failure(x) => new FailureState("", charIndex, x)
				}
			})
			case '-'  => new IntegerState("-", {s:Number =>
				builder.apply[JsonValue](soFar, arrayIndex, JsonValue(s), new IdentityParser[StringOrInt, JsonValue]()) match {
					case Success(x) => new ArrayValueEndState(x, builder, arrayIndex)
					case Failure(x) => new FailureState("", charIndex, x)
				}
			})
			case '.'  => {
				val msg = "Numeric value may not begin with a '.'"
				val ex = new ParseException(msg, charIndex)
				ex.initCause(new NumberFormatException(msg))
				new FailureState(ex);
			}
			case x if ('0' <= x && x <= '9') => new IntegerState("" + x, {s:Number =>
				builder.apply[JsonValue](soFar, arrayIndex, JsonValue(s), new IdentityParser[StringOrInt, JsonValue]()) match {
					case Success(x) => new ArrayValueEndState(x, builder, arrayIndex)
					case Failure(x) => new FailureState("", charIndex, x)
				}
			})
			case x if ('a' <= x && x <= 'z') => new KeywordState("" + x, {s:JsonValue =>
				builder.apply[JsonValue](soFar, arrayIndex, s, new IdentityParser[StringOrInt, JsonValue]()) match {
					case Success(x) => new ArrayValueEndState(x, builder, arrayIndex)
					case Failure(x) => new FailureState("", charIndex, x)
				}
			})
			case _ =>
				new FailureState( new ParseException("Expecting start of value; found " + c, charIndex) )
		}
	}
	
	private[this] class ArrayValueEndState[A](soFar:A, builder:Builder[StringOrInt, JsonValue, A], arrayIndex:Int) extends State[A] {
		def apply(c:Char, charIndex:Int):State[A] = c match {
			case x if x.isWhitespace => this
			case ','  => new ArrayValueStartState(soFar, builder, arrayIndex + 1)
			case ']'  => new EndState(soFar)
			case _    => new FailureState(new ParseException("Expecting ',' or ']'; found " + c, charIndex))
		}
	}
	
	private[this] case class InnerObjectState[A](str:String, pop:(String => State[A]), nesting:Int = 1) extends State[A] {
		def apply(c:Char, index:Int):State[A] = c match {
			case '[' => this.copy(str = str + c, nesting = nesting + 1)
			case '{' => this.copy(str = str + c, nesting = nesting + 1)
			case ']' => if (nesting == 1) {this.pop(str + c)} else {this.copy(str = str + c, nesting = nesting - 1)}
			case '}' => if (nesting == 1) {this.pop(str + c)} else {this.copy(str = str + c, nesting = nesting - 1)}
			case _ => this.copy(str = str + c)
		}
	}
	
	private[this] class StringState[A](str:String, pop:(String => State[A])) extends State[A] {
		def apply(c:Char, charIndex:Int):State[A] = {
			if (c < ' ') {
				new FailureState(new ParseException("Control chars not allowed in strings", charIndex))
			} else if (c == '\\') {
				new StringEscapeState(str, pop)
			} else if (c == '"') {
				pop(str)
			} else {
				new StringState(str + c, pop)
			}
		}
	}
	
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
			case _    => new FailureState(new ParseException("Unexpected escape code in string: " + c, charIndex))
		}
	}
	
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
				new FailureState(new ParseException("non-hex character in unicode escape: " + c, charIndex))
			}
		}
	}
	
	private[this] class IntegerState[A](str:String, pop:(Number => State[A])) extends State[A] {
		def apply(c:Char, charIndex:Int):State[A] = {
			if (c == '}' || c == ']' || c == ',') {
				val rawNewValue = str.toString.trim
				try {
					val newValue:Number = scala.math.BigDecimal(rawNewValue)
					pop(newValue).apply(c, charIndex)
				} catch {
					case _:NumberFormatException => {
						new FailureState(new ParseException("Not a number: " + rawNewValue, charIndex))
					}
				}
			} else {
				new IntegerState(str + c, pop)
			}
		}
	}
	
	private[this] class KeywordState[A](str:String, pop:(JsonValue => State[A])) extends State[A] {
		def apply(c:Char, charIndex:Int):State[A] = {
			if (c == '}' || c == ']' || c == ',') {
				val valueString = str.toString.trim
				val value = valueString match {
					case "true"  => Right(JsonValue(true))
					case "false" => Right(JsonValue(false))
					case "null"  => Right(JsonValue.JsonValueNull)
					case _ => Left(new ParseException("Unexpected keyword: " + valueString, charIndex))
				}
				value match {
					case Right(x) => pop(x).apply(c, charIndex)
					case Left(x) => new FailureState(x)
				}
			} else {
				new KeywordState(str + c, pop)
			}
		}
	}
	
	private[this] class FailureState[A](val ex:ParseException) extends State[A] {
		def this(msg:String, index:Int, cause:Throwable) = this({val a = new ParseException(msg, index); a.initCause(cause); a})
		def apply(c:Char, charIndex:Int):State[A] = this
	}
}
