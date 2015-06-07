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
 * A streaming decoder for json data.
 * This parser is intended to be relatively strict.
 * 
 * == Primitive types ==
 * 
 - null
 - java.lang.Long
 - java.lang.Double
 - java.lang.String
 - java.lang.Boolean
 * 
 * @see [[http://json.org/]]
 *
 * @constructor
 * Creates a JsonParser instance.
 * @param topBuilder the builder that this parser will use when constructing objects
 */
final class JsonParser[A](topBuilder:Builder[A]) {
	private final case class StackFrame[B](soFar:B, builder:Builder[B], state:State) {
		def build(key:String, v:Any):StackFrame[B] = this.copy(soFar = builder.apply(soFar, key, v))
	}
	private final implicit class EditTopFrameStack(a:List[StackFrame[_ >: A]]){
		def replaceTopState(b:State):List[StackFrame[_ >: A]] = a.head.copy(state = b) :: a.tail
		def buildTop(k:String, v:Any):List[StackFrame[_ >: A]] = a.head.build(k, v) :: a.tail
		def pushChild(k:String, s:State):List[StackFrame[_ >: A]] = {
			val b = a.head.builder.childBuilder(k)
			val e = b.asInstanceOf[Builder[Any]]
			val c = StackFrame(e.init, e, s)
			c :: a
		}
	}
	// private implicit def stack2EditTopFrameStack(a:List[StackFrame[_ >: A]]):EditTopFrameStack = new EditTopFrameStack(a)
	
	private object SingletonBuilder extends Builder[Any] {
		val init:String = ""
		def apply(folding:Any, key:String, value:Any):Any = value
		def childBuilder(key:String):Builder[Any] = this
		val resultType:Class[Any] = classOf[Any]
		override def toString:String = "SingletonBuilder"
	}
	
	
	
	/**
	 * Decodes the input values to an object.
	 * @param chars the serialized json object or array
	 * @return the parsed object
	 */
	def parse(chars:Iterable[Char]):A = {
		topBuilder.resultType.cast({
			var length = 0;
			val a = chars.zipWithIndex.foldLeft[List[StackFrame[_ >: A]]](
				List(StackFrame(topBuilder.init, topBuilder, InitState), StackFrame(topBuilder.init, SingletonBuilder, TopState))
			){(stateStack:List[StackFrame[_ >: A]], charIndex:(Char, Int)) =>
				val (char2, index) = charIndex
				val char = if (char2.isWhitespace) {' '} else {char2}
				
				val retVal = stateStack.head.state.apply(stateStack, char, index)
				
				length = index + 1;
				retVal
			}
			if (a.lengthCompare(1) == 0) {
				a.head.soFar
			} else {
				throw new ParseException("Incomplete object", length);
			}
		})
	}
	
	def parse(chars:java.io.Reader):A = this.parse(new Reader2Iterable(chars))
	
	
	/** The parser's state */
	private trait State {
		def apply(in:List[StackFrame[_ >: A]], c:Char, index:Int):List[StackFrame[_ >: A]]
	}
	
	private object TopState extends State {
		def apply(in:List[StackFrame[_ >: A]], c:Char, index:Int):List[StackFrame[_ >: A]] =  c match {
			case x if x.isWhitespace => in
			case _    => throw new ParseException("At end of item, but; found " + c, index)
		}
		override def toString:String = "TopState"
	}
	
	private object InitState extends State {
		def apply(in:List[StackFrame[_ >: A]], c:Char, index:Int):List[StackFrame[_ >: A]] = c match {
			case '\ufeff' => in // byte-order-mark
			case x if x.isWhitespace => in
			case '{'  => in.replaceTopState(new ObjectKeyStartState(""))
			case '['  => in.replaceTopState(new ArrayValueStartState(""))
			case _    => throw new ParseException("Expecting '{' or '['; found " + c, index)
		}
		override def toString:String = "InitState"
	}
	
	private class ObjectKeyStartState(parKey:String) extends State {
		def apply(in:List[StackFrame[_ >: A]], c:Char, index:Int):List[StackFrame[_ >: A]] = c match {
			case x if x.isWhitespace => in
			case '"'  => (new StackFrame(StringBuilder.init, StringBuilder.asInstanceOf[Builder[Any]], new StringState(""))) :: (new StackFrame("", SingletonBuilder, new ObjectKeyEndState(parKey))) :: in.replaceTopState(new ObjectKeyEndState(parKey))
			case '}'  => in.tail.buildTop(parKey, in.head.soFar)
			case _    => throw new ParseException("Expecting start of key; found " + c, index)
		}
		override def toString:String = "ObjectKeyStartState(" + parKey + ")"
	}
	
	private class ObjectKeyEndState(parKey:String) extends State {
		def apply(in:List[StackFrame[_ >: A]], c:Char, index:Int):List[StackFrame[_ >: A]] = c match {
			case x if x.isWhitespace => in
			case ':'  => {
				in.tail.replaceTopState(new ObjectValueStartState(parKey, in.head.soFar.toString))
			}
			case _    => throw new ParseException("Expecting ':'; found " + c, index)
		}
		override def toString:String = "ObjectKeyEndState(" + parKey + ")"
	}
	
	private class ObjectValueStartState(parKey:String, currKey:String) extends State {
		def apply(in:List[StackFrame[_ >: A]], c:Char, index:Int):List[StackFrame[_ >: A]] = c match {
			case x if x.isWhitespace => in
			case '"'  => (new StackFrame(StringBuilder.init, StringBuilder.asInstanceOf[Builder[Any]], new StringState(currKey))) :: in.replaceTopState(new ObjectValueEndState(parKey, currKey))
			case '['  => in.replaceTopState(new ObjectValueEndState(parKey, currKey)).pushChild(currKey, new ArrayValueStartState(currKey))
			case '{'  => in.replaceTopState(new ObjectValueEndState(parKey, currKey)).pushChild(currKey, new ObjectKeyStartState(currKey))
			case '-'  => (new StackFrame(StringBuilder.init + c, StringBuilder.asInstanceOf[Builder[Any]], new IntegerState(currKey))) :: in.replaceTopState(new ObjectValueEndState(parKey, currKey))
			case x if ('0' <= x && x <= '9') =>
					(new StackFrame(StringBuilder.init + c, StringBuilder.asInstanceOf[Builder[Any]], new IntegerState(currKey))) :: in.replaceTopState(new ObjectValueEndState(parKey, currKey))
			case x if ('a' <= x && x <= 'z') =>
					(new StackFrame(StringBuilder.init + c, StringBuilder.asInstanceOf[Builder[Any]], new KeywordState(currKey))) :: in.replaceTopState(new ObjectValueEndState(parKey, currKey))
			case _ => throw new ParseException("Expecting start of value; found " + c, index)
		}
		override def toString:String = "ObjectValueStartState(" + parKey + "," + currKey + ")"
	}
	
	private class ObjectValueEndState(parKey:String, currKey:String) extends State {
		def apply(in:List[StackFrame[_ >: A]], c:Char, charIndex:Int):List[StackFrame[_ >: A]] = c match {
			case x if x.isWhitespace => in
			case ','  => in.replaceTopState(new ObjectKeyStartState(parKey))
			case '}'  => in.tail.buildTop(parKey, in.head.soFar)
			case _    => throw new ParseException("Expecting ',' or ']'; found " + c, charIndex)
		}
		override def toString:String = "ObjectValueEndState(" + parKey + "," + currKey + ")"
	}
	
	
	private class ArrayValueStartState(parKey:String, arrayIndex:Int = 0) extends State {
		def apply(in:List[StackFrame[_ >: A]], c:Char, charIndex:Int):List[StackFrame[_ >: A]] = c match {
			case x if x.isWhitespace => in
			case ']'  => in.tail.buildTop(parKey, in.head.soFar)
			case '"'  => (new StackFrame(StringBuilder.init, StringBuilder.asInstanceOf[Builder[Any]], new StringState(arrayIndex.toString))) :: in.replaceTopState(new ArrayValueEndState(parKey, arrayIndex))
			case '['  => in.replaceTopState(new ArrayValueEndState(parKey, arrayIndex)).pushChild(arrayIndex.toString, new ArrayValueStartState(arrayIndex.toString))
			case '{'  => in.replaceTopState(new ArrayValueEndState(parKey, arrayIndex)).pushChild(arrayIndex.toString, new ObjectKeyStartState(arrayIndex.toString))
			case '-'  => (new StackFrame(StringBuilder.init + c, StringBuilder.asInstanceOf[Builder[Any]], new IntegerState(arrayIndex.toString))) :: in.replaceTopState(new ArrayValueEndState(parKey, arrayIndex))
			case x if ('0' <= x && x <= '9') =>
					(new StackFrame(StringBuilder.init + c, StringBuilder.asInstanceOf[Builder[Any]], new IntegerState(arrayIndex.toString))) :: in.replaceTopState(new ArrayValueEndState(parKey, arrayIndex))
			case x if ('a' <= x && x <= 'z') =>
					(new StackFrame(StringBuilder.init + c, StringBuilder.asInstanceOf[Builder[Any]], new KeywordState(arrayIndex.toString))) :: in.replaceTopState(new ArrayValueEndState(parKey, arrayIndex))
			case _ =>
					throw new ParseException("Expecting start of value; found " + c, charIndex)
		}
		override def toString:String = "ArrayValueStartState(" + parKey + "," + arrayIndex + ")"
	}
	
	private class ArrayValueEndState(parKey:String, arrayIndex:Int) extends State {
		def apply(in:List[StackFrame[_ >: A]], c:Char, charIndex:Int):List[StackFrame[_ >: A]] = c match {
			case x if x.isWhitespace => in
			case ','  => in.replaceTopState(new ArrayValueStartState(parKey, arrayIndex + 1))
			case ']'  => in.tail.buildTop(parKey, in.head.soFar)
			case _    => throw new ParseException("Expecting ',' or ']'; found " + c, charIndex)
		}
		override def toString:String = "ArrayValueEndState(" + parKey + "," + arrayIndex + ")"
	}
	
	private class StringState(key:String) extends State {
		def apply(in:List[StackFrame[_ >: A]], c:Char, charIndex:Int):List[StackFrame[_ >: A]] = {
			if (c < ' ') {
				throw new ParseException("Control chars not allowed in strings", charIndex)
			} else if (c == '\\') {
				in.replaceTopState(new StringEscapeState(key))
			} else if (c == '"') {
				val newValue = in.head.soFar
				in.tail.buildTop(key, newValue)
			} else {
				in.buildTop("", c)
			}
		}
		override def toString:String = "StringState(" + key + ")"
	}
	
	private class StringEscapeState(key:String) extends State {
		def apply(in:List[StackFrame[_ >: A]], c:Char, charIndex:Int):List[StackFrame[_ >: A]] = c match {
			case '"'  => in.buildTop("", "\"").replaceTopState(new StringState(key))
			case '\\' => in.buildTop("", "\\").replaceTopState(new StringState(key))
			case '/'  => in.buildTop("", "/" ).replaceTopState(new StringState(key))
			case 'b'  => in.buildTop("", "\b").replaceTopState(new StringState(key))
			case 'f'  => in.buildTop("", "\f").replaceTopState(new StringState(key))
			case 'n'  => in.buildTop("", "\n").replaceTopState(new StringState(key))
			case 'r'  => in.buildTop("", "\r").replaceTopState(new StringState(key))
			case 't'  => in.buildTop("", "\t").replaceTopState(new StringState(key))
			case 'u'  => in.replaceTopState(new StringUnicodeEscapeState(key))
			case _    => throw new ParseException("Unexpected escape code in string: " + c, charIndex)
		}
		override def toString:String = "StringEscapeState(" + key + ")"
	}
	
	private class StringUnicodeEscapeState(key:String, characters:Int = 0, value:Int = 0) extends State {
		def apply(in:List[StackFrame[_ >: A]], c:Char, charIndex:Int):List[StackFrame[_ >: A]] = {
			if (('0' <= c && c <= '9') || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F')) {
				val cInt = java.lang.Integer.parseInt("" + c, 16)
				val newValue = value * 16 + cInt
				
				if (characters < 3) {
					in.replaceTopState(new StringUnicodeEscapeState(key, characters + 1, value * 16 + cInt))
				} else {
					in.buildTop("", newValue.toChar).replaceTopState(new StringState(key))
				}
			} else {
				throw new ParseException("non-hex character in unicode escape: " + c, charIndex)
			}
		}
		override def toString:String = "StringUnicodeEscapeState(" + key + ")"
	}
	
	private class IntegerState(key:String) extends State {
		def apply(in:List[StackFrame[_ >: A]], c:Char, charIndex:Int):List[StackFrame[_ >: A]] = {
			if (c == '}' || c == ']' || c == ',') {
				val newValue = (in.head.soFar).toString.trim.toLong
				val in2 = in.tail.buildTop(key, newValue)
				in2.head.state.apply(in2, c, charIndex)
			} else {
				in.buildTop("", c)
			}
		}
		override def toString:String = "IntegerState(" + key + ")"
	}
	
	private class KeywordState(key:String) extends State {
		def apply(in:List[StackFrame[_ >: A]], c:Char, charIndex:Int):List[StackFrame[_ >: A]] = {
			if (c == '}' || c == ']' || c == ',') {
				val valueString = (in.head.soFar).toString.trim
				val value = valueString match {
					case "true"  => true
					case "false" => false
					case "null"  => null
					case _ => throw new ParseException("Unexpected keyword: " + valueString, charIndex)
				}
				val in2 = in.tail.buildTop(key, value)
				in2.head.state.apply(in2, c, charIndex)
			} else {
				in.buildTop("", c)
			}
		}
		override def toString:String = "IntegerState(" + key + ")"
	}
	
	/** A builder that creates strings */
	private object StringBuilder extends Builder[String] {
		val init:String = ""
		def apply(folding:String, key:String, value:Any):String = {
			folding + value.toString
		}
		def childBuilder(key:String):Builder[String] = this
		val resultType:Class[String] = classOf[String]
	}
}

