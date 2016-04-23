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
 * A streaming decoder for csv data.
 * 
 * This parser is lenient, in that it ignores trailing delimiters
 * 
 * A CSV file is always two levels deep - a two dimensional array.
 * 
 * @constructor
 * Creates a CsvParser instance.
 * @param meaningfulCharacters determines which characters have special meanings
 */
final class CsvParser(
		meaningfulCharacters:CsvParser.CharacterMeanings = CsvParser.csvCharacterMeanings
) extends Parser[Int, String, Iterable[Char]] {
	
	def parseEither[A](builder:Builder[Int, String, A], chars:Iterable[Char]):Either[A,String] = {
		Left(this.parseComplex(builder, chars))
	}
	
	/**
	 * Decodes the input values to an object.
	 * @tparam A the type of object to create
	 * @param builder the builder to use to construct the object
	 * @param chars the serialized json object or array
	 * @return the parsed object
	 */
	def parseComplex[A](builder:Builder[Int, String, A], chars:Iterable[Char]):A = {
		val endState = chars.zipWithIndex.foldLeft(State(builder.init, 0, "", "", false, false)){(state, ci) => 
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
				new State(
					value = builder.apply(state.innerIndex).apply(state.value, state.innerInput, new LineParser),
					innerIndex = state.innerIndex + 1,
					innerInput = "",
					endingWhitespace = "",
					quoted = false,
					escaped = false
				)
			} else {
				state.appendChar(char)
			}
		}
		
		if (endState.innerInput.isEmpty) {
			endState.value
		} else {
			builder.apply(endState.innerIndex).apply(endState.value, endState.innerInput, new LineParser)
		}
	}
	
	/**
	 * Decodes the input values to an object.
	 * @tparam A the type of object to create
	 * @param builder the builder to use to construct the object
	 * @param chars the serialized json object or array
	 * @return the parsed object
	 */
	def parseComplex[A](builder:Builder[Int, String, A], chars:java.io.Reader):A = this.parseComplex(builder, new Reader2Iterable(chars))
	
	/**
	 * Decodes the input values to an object.
	 * @param chars the serialized json object or array
	 * @return the parsed object
	 */
	def parseEither[A](builder:Builder[Int, String, A], chars:java.io.Reader):Either[A,String] = this.parseEither(builder, new Reader2Iterable(chars))
	
	
	private[this] case class State[A] (
		value:A,
		innerIndex:Int,
		innerInput:String,
		endingWhitespace:String,
		quoted:Boolean,
		escaped:Boolean
	) {
		def appendChar(c:Char):State[A] = this.copy(endingWhitespace = "", innerInput = innerInput + endingWhitespace + c)
	}
	
	
	/** Splits a CSV record (i.e. one line) into fields */
	private[this] final class LineParser extends Parser[Int, String, String] {
		def parseEither[A](builder:Builder[Int, String, A], chars:String):Either[A,String] = {
			Left(this.parseComplex(builder, chars))
		}
		
		def parseComplex[A](builder:Builder[Int, String, A], chars:String):A = {
			val endState = chars.zipWithIndex.foldLeft(State(builder.init, 0, "", "", false, false)){(state, ci) => 
				val (char, index) = ci
				
				if (state.escaped) {
					state.appendChar(char).copy(escaped = false)
				} else if (state.quoted) {
					val isQuote = meaningfulCharacters.stringDelimeter contains char;
					if (isQuote) {state.copy(quoted = false)}
					else {state.appendChar(char)}
				} else if (meaningfulCharacters.ignorable contains char) {
					if (state.innerInput == "") {
						state
					} else {
						state.copy(endingWhitespace = state.endingWhitespace + char)
					}
				} else if (meaningfulCharacters.escape contains char) {
					state.copy(escaped = true)
				} else if (meaningfulCharacters.stringDelimeter contains char) {
					state.copy(quoted = true)
				} else if (meaningfulCharacters.fieldDelimeter contains char) {
					new State(
						value = builder.apply(state.innerIndex).apply(state.value, state.innerInput, new IdentityParser),
						innerIndex = state.innerIndex + 1,
						innerInput = "",
						endingWhitespace = "",
						quoted = false,
						escaped = false
					)
				} else {
					state.appendChar(char)
				}
			}
			
			if (endState.innerInput.isEmpty) {
				endState.value
			} else {
				builder.apply(endState.innerIndex).apply(endState.value, endState.innerInput, new IdentityParser)
			}
		}
	}
}

/**
 * Contains classes used to customize the CsvParser's behavior, as
 * well as a few common instances of those classes.
 */
object CsvParser {
	/**
	 * A data container which tells the CsvParser which characters are special
	 * 
	 * @constructor
	 * @param recordDelimeter first-level separators; separate records
	 * @param fieldDelimeter second-level separators; separate the fields within a record
	 * @param stringDelimeter A character that starts and ends strings of literal characters
	 * @param ignorable characters that are trimmed from the start or end of a record
	 * @param escape a character that causes the next character to be interpreted literally
	 */
	final case class CharacterMeanings(
			val recordDelimeter:Set[Char],
			val fieldDelimeter:Set[Char],
			val stringDelimeter:Set[Char],
			val ignorable:Set[Char],
			val escape:Set[Char]
	)
	
	/**
	 * A CharacterMeanings that uses a set of characters similar to most Comma-Separated-Values files
	 */
	val csvCharacterMeanings = CharacterMeanings(Set('\n'), Set(','), Set('"'), Set(' ', '\t', '\uFEFF'), Set('\\'))
	
	/**
	 * A CharacterMeanings that uses a set of characters similar to most Tab-Separated-Values files
	 */
	val tsvCharacterMeanings = CharacterMeanings(Set('\n'), Set('\t'), Set('"'), Set(' ', '\uFEFF'), Set('\\'))
	
	/**
	 * A CharacterMeanings that uses ASCII record and field delimiter characters to separate records and fields
	 */
	val asciiCharacterMeanings = CharacterMeanings(Set('\u001E'), Set('\u001F'), Set.empty, Set.empty, Set.empty)
}

