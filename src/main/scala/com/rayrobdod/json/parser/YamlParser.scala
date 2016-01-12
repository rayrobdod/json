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
package com.rayrobdod.json.parser;

import java.text.ParseException
import scala.collection.immutable.{Seq, Map, Stack}
import com.rayrobdod.json.builder._


/**
 * A streaming decoder for yaml data.
 * This parser is intended to be relatively strict.
 * 
 * == Primitive types ==
 * 
 - java.lang.String
 * 
 * @see [[http://yaml.org/]]
 *
 * @constructor
 * Creates a YamlParser instance.
 * @param topBuilder the builder that this parser will use when constructing objects
 */
final class YamlParser[A](topBuilder:Builder[A]) {
	private[this] final case class StackFrame[B](soFar:B, builder:Builder[B], state:State) {
		def build(key:String, v:Any):StackFrame[B] = this.copy(soFar = builder.apply(soFar, key, v))
	}
	private[this] final implicit class EditTopFrameStack(a:List[StackFrame[_ >: A]]){
		def replaceTopState(b:State):List[StackFrame[_ >: A]] = a.head.copy(state = b) :: a.tail
		def buildTop(k:String, v:Any):List[StackFrame[_ >: A]] = a.head.build(k, v) :: a.tail
		def pushChild(k:String, s:State):List[StackFrame[_ >: A]] = {
			val b = a.head.builder.childBuilder(k)
			val e = b.asInstanceOf[Builder[Any]]
			val c = StackFrame(e.init, e, s)
			c :: a
		}
	}
	
	private[this] object SingletonBuilder extends Builder[Any] {
		val init:String = ""
		def apply(folding:Any, key:String, value:Any):Any = value
		def childBuilder(key:String):Builder[Any] = this
		val resultType:Class[Any] = classOf[Any]
	}
	
	
	
	/**
	 * Decodes the input values to an object.
	 * @param chars the serialized json object or array
	 * @return the parsed object
	 */
	 def parse(chars:Iterable[Char]):Any = {
	 	 var length = 0;
	 	 val a = chars.zipWithIndex.foldLeft[List[StackFrame[_ >: A]]](
	 	 		List(StackFrame(topBuilder.init, SingletonBuilder, InitState), StackFrame("", SingletonBuilder, TopState))
	 	 ){(stateStack:List[StackFrame[_ >: A]], charIndex:(Char, Int)) =>
	 	 	 val (char, index) = charIndex
	 	 	 
	 	 	 val retVal = stateStack.head.state.apply(stateStack, char, index)
	 	 	 
	 	 	 length = index + 1;
	 	 	 retVal
	 	 }
	 	 if (a.lengthCompare(1) < 0) {
	 	 	 throw new ParseException("stack lost", -1)
	 	 }
	 	 var b = a
	 	 while (b.lengthCompare(1) > 0) {
	 	 	 b = b.head.state.forceFlatten(b)
	 	 }
	 	 b.head.soFar
	 }
	
	/**
	 * Decodes the input values to an object.
	 * @param chars the serialized json object or array
	 * @return the parsed object
	 */
	def parse(chars:java.io.Reader):Any = this.parse(new Reader2Iterable(chars))
	
	
	/** The parser's state. To be placed inside a foldleft. */
	private[this] trait State {
		def apply(in:List[StackFrame[_ >: A]], c:Char, index:Int):List[StackFrame[_ >: A]]
		def forceFlatten(in:List[StackFrame[_ >: A]]):List[StackFrame[_ >: A]]
	}
	
	private[this] object TopState extends State {
		def apply(in:List[StackFrame[_ >: A]], c:Char, index:Int):List[StackFrame[_ >: A]] =  c match {
			case x if x.isWhitespace => in
			case _    => in
		}
		def forceFlatten(in:List[StackFrame[_ >: A]]) = in.tail.buildTop("", in.head.soFar)
	}
	
	private[this] object InitState extends State {
		def apply(in:List[StackFrame[_ >: A]], c:Char, index:Int):List[StackFrame[_ >: A]] = c match {
			case '\ufeff' => in // byte-order-mark
			case '#' => new StackFrame("", SingletonBuilder, CommentState) :: in
			case x if x.isWhitespace => in
			case '%' => throw new ParseException("Directives not supported", index)
			case '"' => new StackFrame(StringBuilder.init, StringBuilder, new DoubleQuotedStringState("")) :: in.tail
			case '\'' => new StackFrame(StringBuilder.init, StringBuilder, new SingleQuotedStringState("")) :: in.tail
			case _   => throw new ParseException("Content not supported", index)
		}
		def forceFlatten(in:List[StackFrame[_ >: A]]) = in.tail
	}
	
	private[this] object CommentState extends State {
		def apply(in:List[StackFrame[_ >: A]], c:Char, index:Int):List[StackFrame[_ >: A]] = c match {
			case '\n' => in.tail
			case '\r' => in.tail
			case _    => in
		}
		def forceFlatten(in:List[StackFrame[_ >: A]]) = in.tail
	}
	
	private[this] class SingleQuoteEscapeBuffer(key:String) extends State {
		def apply(in:List[StackFrame[_ >: A]], c:Char, charIndex:Int):List[StackFrame[_ >: A]] = c match {
			case '\'' => in.buildTop("", "'").replaceTopState(new SingleQuotedStringState(key))
			case x => {
				val newIn = in.tail.buildTop(key, in.head.soFar)
				newIn.head.state.apply(newIn, c, charIndex)
			}
		}
		def forceFlatten(in:List[StackFrame[_ >: A]]) = in.tail.buildTop(key, in.head.soFar)
	}
	
	private[this] class SingleQuotedStringState(key:String) extends State {
		def apply(in:List[StackFrame[_ >: A]], c:Char, charIndex:Int):List[StackFrame[_ >: A]] = c match {
			case x if x < ' ' => throw new ParseException("Control chars not allowed in strings", charIndex)
			case '\'' => in.replaceTopState(new SingleQuoteEscapeBuffer(key))
			case x => in.buildTop("", c)
		}
		def forceFlatten(in:List[StackFrame[_ >: A]]) = in.tail.buildTop(key, in.head.soFar)
	}
	
	private[this] class DoubleQuotedStringState(key:String) extends State {
		def apply(in:List[StackFrame[_ >: A]], c:Char, charIndex:Int):List[StackFrame[_ >: A]] = c match {
			case x if x < ' ' => throw new ParseException("Control chars not allowed in strings", charIndex)
			case '\\' => in.replaceTopState(new DoubleQuotedStringEscapeState(key))
			case '"' => {
				val newValue = in.head.soFar
				in.tail.buildTop(key, newValue)
			}
			case x => in.buildTop("", c)
		}
		def forceFlatten(in:List[StackFrame[_ >: A]]) = in.tail.buildTop(key, in.head.soFar)
	}
	
	private[this] class DoubleQuotedStringEscapeState(key:String) extends State {
		def apply(in:List[StackFrame[_ >: A]], c:Char, charIndex:Int):List[StackFrame[_ >: A]] = c match {
			case '0'  => in.buildTop("", "\u0000").replaceTopState(new DoubleQuotedStringState(key))
			case 'a'  => in.buildTop("", "\u0007").replaceTopState(new DoubleQuotedStringState(key))
			case 'b'  => in.buildTop("", "\b").replaceTopState(new DoubleQuotedStringState(key))
			case 't'  => in.buildTop("", "\t").replaceTopState(new DoubleQuotedStringState(key))
			case 'n'  => in.buildTop("", "\n").replaceTopState(new DoubleQuotedStringState(key))
			case 'v'  => in.buildTop("", "\u000B").replaceTopState(new DoubleQuotedStringState(key))
			case 'f'  => in.buildTop("", "\f").replaceTopState(new DoubleQuotedStringState(key))
			case 'r'  => in.buildTop("", "\r").replaceTopState(new DoubleQuotedStringState(key))
			case 'e'  => in.buildTop("", "\u001B").replaceTopState(new DoubleQuotedStringState(key))
			case ' '  => in.buildTop("", " ").replaceTopState(new DoubleQuotedStringState(key))
			case '"'  => in.buildTop("", "\"").replaceTopState(new DoubleQuotedStringState(key))
			case '/'  => in.buildTop("", "/" ).replaceTopState(new DoubleQuotedStringState(key))
			case '\\' => in.buildTop("", "\\").replaceTopState(new DoubleQuotedStringState(key))
			case 'N'  => in.buildTop("", "\u0085").replaceTopState(new DoubleQuotedStringState(key))
			case '_'  => in.buildTop("", "\u00A0").replaceTopState(new DoubleQuotedStringState(key))
			case 'L'  => in.buildTop("", "\u2028").replaceTopState(new DoubleQuotedStringState(key))
			case 'P'  => in.buildTop("", "\u2029").replaceTopState(new DoubleQuotedStringState(key))
			case 'x'  => throw new ParseException("two-character escape", charIndex)
			case 'u'  => in.replaceTopState(new DoubleQuotedStringUnicodeEscapeState(key))
			case 'U'  => throw new ParseException("eight-character escape", charIndex)
			case _    => throw new ParseException("Unexpected escape code in string: " + c, charIndex)
		}
		def forceFlatten(in:List[StackFrame[_ >: A]]) = throw new ParseException("document ends in incomplete escape", -1)
	}
	
	private[this] class DoubleQuotedStringUnicodeEscapeState(key:String, characters:Int = 0, value:Int = 0) extends State {
		def apply(in:List[StackFrame[_ >: A]], c:Char, charIndex:Int):List[StackFrame[_ >: A]] = {
			if (('0' <= c && c <= '9') || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F')) {
				val cInt = java.lang.Integer.parseInt("" + c, 16)
				val newValue = value * 16 + cInt
				
				if (characters < 3) {
					in.replaceTopState(new DoubleQuotedStringUnicodeEscapeState(key, characters + 1, value * 16 + cInt))
				} else {
					in.buildTop("", newValue.toChar).replaceTopState(new DoubleQuotedStringState(key))
				}
			} else {
				throw new ParseException("non-hex character in unicode escape: " + c, charIndex)
			}
		}
		def forceFlatten(in:List[StackFrame[_ >: A]]) = throw new ParseException("document ends in incomplete escape", -1)
	}
	
	
	
	/** A builder that creates strings */
	private[this] object StringBuilder extends Builder[Any] {
		val init:String = ""
		def apply(folding:Any, key:String, value:Any):String = {
			folding.toString + value.toString
		}
		def childBuilder(key:String):Builder[_] = this
		val resultType:Class[Any] = classOf[Any]
	}
}
