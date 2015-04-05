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
 */
final class JsonParser[A](topBuilder:Builder[A]) {
	private final case class StackFrame[B](soFar:B, builder:Builder[B], state:State) {
		def build(key:String, v:Any):StackFrame[B] = this.copy(soFar = builder.apply(soFar, key, v))
	}
	private final implicit class EditTopFrameStack(a:Stack[StackFrame[_ >: A]]){
		def replaceTopState(b:State):Stack[StackFrame[_ >: A]] = a.pop.push(a.top.copy(state = b))
		def buildTop(k:String, v:Any):Stack[StackFrame[_ >: A]] = a.pop.push(a.top.build(k, v))
		def pushChild(k:String, s:State):Stack[StackFrame[_ >: A]] = {
			val b = a.top.builder.childBuilder(k)
			val e = b.asInstanceOf[Builder[Any]]
			val c = StackFrame(e.init, e, s)
			a.push(c)
		}
	}
	// private implicit def stack2EditTopFrameStack(a:Stack[StackFrame[_ >: A]]):EditTopFrameStack = new EditTopFrameStack(a)
	
	private object SingletonBuilder extends Builder[Any] {
		val init:String = ""
		def apply(folding:Any, key:String, value:Any) = value
		def childBuilder(key:String):Builder[Any] = this
		val resultType:Class[Any] = classOf[Any]
		override def toString = "SingletonBuilder"
	}
	
	
	
	/**
	 * Decodes the input values to an object.
	 */
	def parse(chars:Iterable[Char]):A = {
		topBuilder.resultType.cast({
			var length = 0;
			val a = chars.zipWithIndex.foldLeft[Stack[StackFrame[_ >: A]]](
				Stack(StackFrame(topBuilder.init, topBuilder, InitState), StackFrame(topBuilder.init, SingletonBuilder, TopState))
			){(stateStack:Stack[StackFrame[_ >: A]], charIndex:(Char, Int)) =>
				val (char2, index) = charIndex
				val char = if (char2.isWhitespace) {' '} else {char2}
				
				val retVal = stateStack.top.state.apply(stateStack, char, index)
				
				// System.out.println()
				// System.out.println(retVal)
				length = index + 1;
				retVal
			}
			if (a.lengthCompare(1) == 0) {
				a.top.soFar
			} else {
				throw new ParseException("Incomplete object", length);
			}
		})
	}
	
	
	
	/** The parser's state */
	private trait State {
		def apply(in:Stack[StackFrame[_ >: A]], c:Char, index:Int):Stack[StackFrame[_ >: A]]
	}
	
	private object TopState extends State {
		def apply(in:Stack[StackFrame[_ >: A]], c:Char, index:Int) =  c match {
			case ' '  => in
			case '\t' => in
			case '\n' => in
			case '\r' => in
			case _    => throw new ParseException("At end of item, but; found " + c, index)
		}
		override def toString = "TopState"
	}
	
	private object InitState extends State {
		def apply(in:Stack[StackFrame[_ >: A]], c:Char, index:Int):Stack[StackFrame[_ >: A]] = c match {
			case ' '  => in
			case '\t' => in
			case '\n' => in
			case '\r' => in
			case '{'  => in.replaceTopState(new ObjectKeyStartState(""))
			case '['  => in.replaceTopState(new ArrayValueStartState(""))
			case _    => throw new ParseException("Expecting '{' or '['; found " + c, index)
		}
		override def toString = "InitState"
	}
	
	private class ObjectKeyStartState(parKey:String) extends State {
		def apply(in:Stack[StackFrame[_ >: A]], c:Char, index:Int):Stack[StackFrame[_ >: A]] = c match {
			case ' '  => in
			case '\t' => in
			case '\n' => in
			case '\r' => in
			case '"'  => in.replaceTopState(new ObjectKeyEndState(parKey)).push(new StackFrame("", SingletonBuilder, new ObjectKeyEndState(parKey))).push(new StackFrame(StringBuilder.init, StringBuilder.asInstanceOf[Builder[Any]], new StringState("")))
			case '}'  => in.pop.buildTop(parKey, in.top.soFar)
			case _    => throw new ParseException("Expecting start of key; found " + c, index)
		}
		override def toString = "ObjectKeyStartState(" + parKey + ")"
	}
	
	private class ObjectKeyEndState(parKey:String) extends State {
		def apply(in:Stack[StackFrame[_ >: A]], c:Char, index:Int):Stack[StackFrame[_ >: A]] = c match {
			case ' '  => in
			case '\t' => in
			case '\n' => in
			case '\r' => in
			case ':'  => {
				in.pop.replaceTopState(new ObjectValueStartState(parKey, in.top.soFar.toString))
			}
			case _    => throw new ParseException("Expecting ':'; found " + c, index)
		}
		override def toString = "ObjectKeyEndState(" + parKey + ")"
	}
	
	private class ObjectValueStartState(parKey:String, currKey:String) extends State {
		def apply(in:Stack[StackFrame[_ >: A]], c:Char, index:Int):Stack[StackFrame[_ >: A]] = c match {
			case ' '  => in
			case '\t' => in
			case '\n' => in
			case '\r' => in
			case '"'  => in.replaceTopState(new ObjectValueEndState(parKey, currKey)).push(new StackFrame(StringBuilder.init, StringBuilder.asInstanceOf[Builder[Any]], new StringState(currKey)))
			case '['  => in.replaceTopState(new ObjectValueEndState(parKey, currKey)).pushChild(currKey, new ArrayValueStartState(currKey))
			case '{'  => in.replaceTopState(new ObjectValueEndState(parKey, currKey)).pushChild(currKey, new ObjectKeyStartState(currKey))
			case '-'  => in.replaceTopState(new ObjectValueEndState(parKey, currKey)).push(new StackFrame(StringBuilder.init + c, StringBuilder.asInstanceOf[Builder[Any]], new IntegerState(currKey)))
			case _    => {
				if ('0' <= c && c <= '9') {
					in.replaceTopState(new ObjectValueEndState(parKey, currKey)).push(new StackFrame(StringBuilder.init + c, StringBuilder.asInstanceOf[Builder[Any]], new IntegerState(currKey)))
				} else if ('a' <= c && c <= 'z') {
					in.replaceTopState(new ObjectValueEndState(parKey, currKey)).push(new StackFrame(StringBuilder.init + c, StringBuilder.asInstanceOf[Builder[Any]], new KeywordState(currKey)))
				} else {
					throw new ParseException("Expecting start of value; found " + c, index)
				}
			}
		}
		override def toString = "ObjectValueStartState(" + parKey + "," + currKey + ")"
	}
	
	private class ObjectValueEndState(parKey:String, currKey:String) extends State {
		def apply(in:Stack[StackFrame[_ >: A]], c:Char, charIndex:Int):Stack[StackFrame[_ >: A]] = c match {
			case ' '  => in
			case '\t' => in
			case '\n' => in
			case '\r' => in
			case ','  => in.replaceTopState(new ObjectKeyStartState(parKey))
			case '}'  => in.pop.buildTop(parKey, in.top.soFar)
			case _    => throw new ParseException("Expecting ',' or ']'; found " + c, charIndex)
		}
		override def toString = "ObjectValueEndState(" + parKey + "," + currKey + ")"
	}
	
	
	private class ArrayValueStartState(parKey:String, arrayIndex:Int = 0) extends State {
		def apply(in:Stack[StackFrame[_ >: A]], c:Char, charIndex:Int):Stack[StackFrame[_ >: A]] = c match {
			case ' '  => in
			case '\t' => in
			case '\n' => in
			case '\r' => in
			case ']'  => in.pop.buildTop(parKey, in.top.soFar)
			case '"'  => in.replaceTopState(new ArrayValueEndState(parKey, arrayIndex)).push(new StackFrame(StringBuilder.init, StringBuilder.asInstanceOf[Builder[Any]], new StringState(arrayIndex.toString)))
			case '['  => in.replaceTopState(new ArrayValueEndState(parKey, arrayIndex)).pushChild(arrayIndex.toString, new ArrayValueStartState(arrayIndex.toString))
			case '{'  => in.replaceTopState(new ArrayValueEndState(parKey, arrayIndex)).pushChild(arrayIndex.toString, new ObjectKeyStartState(arrayIndex.toString))
			case '-'  => in.replaceTopState(new ArrayValueEndState(parKey, arrayIndex)).push(new StackFrame(StringBuilder.init + c, StringBuilder.asInstanceOf[Builder[Any]], new IntegerState(arrayIndex.toString)))
			case _    => {
				if ('0' <= c && c <= '9') {
					in.replaceTopState(new ArrayValueEndState(parKey, arrayIndex)).push(new StackFrame(StringBuilder.init + c, StringBuilder.asInstanceOf[Builder[Any]], new IntegerState(arrayIndex.toString)))
				} else if ('a' <= c && c <= 'z') {
					in.replaceTopState(new ArrayValueEndState(parKey, arrayIndex)).push(new StackFrame(StringBuilder.init + c, StringBuilder.asInstanceOf[Builder[Any]], new KeywordState(arrayIndex.toString)))
				} else {
					throw new ParseException("Expecting start of value; found " + c, charIndex)
				}
			}
		}
		override def toString = "ArrayValueStartState(" + parKey + "," + arrayIndex + ")"
	}
	
	private class ArrayValueEndState(parKey:String, arrayIndex:Int) extends State {
		def apply(in:Stack[StackFrame[_ >: A]], c:Char, charIndex:Int):Stack[StackFrame[_ >: A]] = c match {
			case ' '  => in
			case '\t' => in
			case '\n' => in
			case '\r' => in
			case ','  => in.replaceTopState(new ArrayValueStartState(parKey, arrayIndex + 1))
			case ']'  => in.pop.buildTop(parKey, in.top.soFar)
			case _    => throw new ParseException("Expecting ',' or ']'; found " + c, charIndex)
		}
		override def toString = "ArrayValueEndState(" + parKey + "," + arrayIndex + ")"
	}
	
	private class StringState(key:String) extends State {
		def apply(in:Stack[StackFrame[_ >: A]], c:Char, charIndex:Int):Stack[StackFrame[_ >: A]] = {
			if (c < ' ') {
				throw new ParseException("Control chars not allowed in strings", charIndex)
			} else if (c == '\\') {
				in.replaceTopState(new StringEscapeState(key))
			} else if (c == '"') {
				val newValue = in.top.soFar
				in.pop.buildTop(key, newValue)
			} else {
				in.buildTop("", c)
			}
		}
		override def toString = "StringState(" + key + ")"
	}
	
	private class StringEscapeState(key:String) extends State {
		def apply(in:Stack[StackFrame[_ >: A]], c:Char, charIndex:Int):Stack[StackFrame[_ >: A]] = c match {
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
		override def toString = "StringEscapeState(" + key + ")"
	}
	
	private class StringUnicodeEscapeState(key:String, characters:Int = 0, value:Int = 0) extends State {
		def apply(in:Stack[StackFrame[_ >: A]], c:Char, charIndex:Int):Stack[StackFrame[_ >: A]] = {
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
		override def toString = "StringUnicodeEscapeState(" + key + ")"
	}
	
	private class IntegerState(key:String) extends State {
		def apply(in:Stack[StackFrame[_ >: A]], c:Char, charIndex:Int):Stack[StackFrame[_ >: A]] = {
			if (c == '}' || c == ']' || c == ',') {
				val newValue = (in.top.soFar).toString.toLong
				val in2 = in.pop.buildTop(key, newValue)
				in2.top.state.apply(in2, c, charIndex)
			} else {
				in.buildTop("", c)
			}
		}
		override def toString = "IntegerState(" + key + ")"
	}
	
	private class KeywordState(key:String) extends State {
		def apply(in:Stack[StackFrame[_ >: A]], c:Char, charIndex:Int):Stack[StackFrame[_ >: A]] = {
			if (c == '}' || c == ']' || c == ',') {
				val valueString = (in.top.soFar).toString.trim
				val value = valueString match {
					case "true"  => true
					case "false" => false
					case "null"  => null
					case _ => throw new ParseException("Unexpected keyword: " + valueString, charIndex)
				}
				val in2 = in.pop.buildTop(key, value)
				in2.top.state.apply(in2, c, charIndex)
			} else {
				in.buildTop("", c)
			}
		}
		override def toString = "IntegerState(" + key + ")"
	}
}

