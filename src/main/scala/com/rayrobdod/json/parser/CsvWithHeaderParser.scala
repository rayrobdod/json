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

import scala.collection.immutable.Seq
import com.rayrobdod.json.builder._
import com.rayrobdod.json.union.{StringOrInt, ParserRetVal}

/**
 * A streaming decoder for csv data, where the first line of the csv data is a header row.
 * 
 * This parser is lenient, in that it ignores trailing delimiters
 * 
 * A CSV file is always two levels deep, an array of key-value mappings.
 * @version 3.0.1
 * 
 * @constructor
 * Creates a CsvParser instance.
 * @param meaningfulCharacters indicates which characters have special meanings
 */
final class CsvWithHeaderParser(
		meaningfulCharacters:CsvParser.CharacterMeanings = CsvParser.csvCharacterMeanings
) extends Parser[StringOrInt, String, Iterable[Char]] {
	private[this] val lineParser = new CsvParser.LineParser(meaningfulCharacters)
	
	/**
	 * Decodes the input values to an object.
	 * @tparam A the type of object to create
	 * @param builder the builder to use to construct the object
	 * @param chars the serialized json object or array
	 * @return the parsed object
	 */
	def parse[A](builder:Builder[StringOrInt, String, A], chars:Iterable[Char]):ParserRetVal[A,Nothing] = {
		val endState = chars.zipWithIndex.foldLeft(State(None, Right(builder.init), 0, "", "", false, false)){(state, ci) => 
			val (char, index) = ci
			
			if (state.escaped) {
				state.appendChar(char).copy(escaped = false)
			} else if (state.quoted) {
				val isQuote = meaningfulCharacters.stringDelimeter contains char;
				state.appendChar(char).copy(quoted = !isQuote)
			} else if (meaningfulCharacters.escape contains char) {
				state.appendChar(char).copy(escaped = true)
			} else if (meaningfulCharacters.stringDelimeter contains char) {
				state.appendChar(char).copy(quoted = true)
			} else if (meaningfulCharacters.recordDelimeter contains char) {
				state.keys.fold{
					val result = lineParser.parse(new PrimitiveSeqBuilder[String], state.innerInput)
					result.fold({x:Seq[String] =>
						new State[A](
							keys = Option(x),
							value = state.value,
							innerIndex = state.innerIndex + 1,
							innerInput = "",
							endingWhitespace = "",
							quoted = false,
							escaped = false
						)
						
					}, {x:Nothing => x}, {(s, i) =>
						new State[A](
							keys = state.keys,
							value = Left(s,i),
							innerIndex = state.innerIndex + 1,
							innerInput = "",
							endingWhitespace = "",
							quoted = false,
							escaped = false
						)
					})
				}{keys:Seq[String] =>
					new State[A](
						keys = state.keys,
						value = state.value.right.flatMap{x =>
							builder.apply(x, state.innerIndex, state.innerInput, lineParser.mapKey[StringOrInt]{i =>
								if (keys.isDefinedAt(i)) {StringOrInt(keys(i))} else {StringOrInt(i)}
							}).left.map{x => ((x._1, x._2 + index))}
						},
						innerIndex = state.innerIndex + 1,
						innerInput = "",
						endingWhitespace = "",
						quoted = false,
						escaped = false
					)
				}
			} else {
				state.appendChar(char)
			}
		}
		
		(if (endState.innerInput.isEmpty) {
			endState.value
		} else {
			endState.keys.fold{
				endState.value
			}{keys =>
				endState.value.right.flatMap{x => 
					builder.apply(x, endState.innerIndex, endState.innerInput, lineParser.mapKey[StringOrInt]{i =>
						if (keys.isDefinedAt(i)) {StringOrInt(keys(i))} else {StringOrInt(i)}
					})
				}
			}
		}).fold({case (msg,idx) => ParserRetVal.Failure(msg,idx)}, {x => ParserRetVal.Complex(x)})
	}
	
	/**
	 * Decodes the input values to an object.
	 * @tparam A the type of object to create
	 * @param builder the builder to use to construct the object
	 * @param chars the serialized json object or array
	 * @return the parsed object
	 */
	def parse[A](builder:Builder[StringOrInt, String, A], chars:java.io.Reader):ParserRetVal[A,String] = this.parse(builder, new Reader2Iterable(chars))
	
	
	private[this] final case class State[A] (
		keys:Option[Seq[String]],
		value:Either[(String,Int),A],
		innerIndex:Int,
		innerInput:String,
		endingWhitespace:String,
		quoted:Boolean,
		escaped:Boolean
	) {
		def appendChar(c:Char):State[A] = this.copy(endingWhitespace = "", innerInput = innerInput + endingWhitespace + c)
	}
}