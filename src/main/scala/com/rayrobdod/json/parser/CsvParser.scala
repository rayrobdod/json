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

import com.rayrobdod.json.builder.Builder
import com.rayrobdod.json.union.ParserRetVal
import com.rayrobdod.json.union.ParserRetVal.{Complex, BuilderFailure, ParserFailure}
import com.rayrobdod.json.union.Failures.Indexed

/**
 * A streaming decoder for csv data.
 * 
 * This parser is lenient, in that it ignores trailing delimiters
 * 
 * A CSV file is always two levels deep - a two dimensional array.
 * @version 4.0
 * 
 * @constructor
 * Creates a CsvParser instance.
 * @param meaningfulCharacters indicates which characters have special meanings
 */
// TODO: location annotation
final class CsvParser(
		meaningfulCharacters:CsvParser.CharacterMeanings = CsvParser.csvCharacterMeanings
) extends Parser[Int, String, Nothing, CountingReader] {
	private[this] val lineParser = new CsvParser.LineParser(meaningfulCharacters)
	
	/**
	 * Decodes the input values to an object.
	 * @tparam A the type of object to create
	 * @param builder the builder to use to construct the object
	 * @param chars the serialized json object or array
	 * @return the parsed object
	 */
	def parse[A, BF](builder:Builder[Int, String, BF, A], chars:Iterable[Char]):ParserRetVal[A, Nothing, Nothing, BF] = {
		this.parse(builder, new Iterator2Reader(chars.iterator))
	}
	
	/**
	 * Decodes the input values to an object.
	 * @tparam A the type of object to create
	 * @param builder the builder to use to construct the object
	 * @param chars the serialized json object or array
	 * @return the parsed object
	 */
	def parse[Result, BF](builder:Builder[Int, String, BF, Result], chars:java.io.Reader):ParserRetVal[Result, Nothing, Nothing, BF] = {
		this.parse(builder, new CountingReader(chars))
	}
	
	def parse[Result, BF](builder:Builder[Int, String, BF, Result], chars:CountingReader):ParserRetVal[Result, Nothing, Nothing, BF] = {
		@scala.annotation.tailrec
		def dothing(rowIdx:Int, folding:builder.Middle):ParserRetVal[builder.Middle, Nothing, Nothing, BF] = {
			sealed trait ThingToDo
			final case class Recurse(nextFolding:builder.Middle) extends ThingToDo
			object ReturnFolding extends ThingToDo
			final case class ReturnFailure(err:BF) extends ThingToDo
			
			val thingToDo = try {
				// check that there are more characters to read; throw if no such characters
				// I have to do this as readers don't have a hasNext or similar method
				chars.read()
				chars.goBackOne()
				val rowStartCharIndex = chars.index + 1
				
				builder.apply(folding, rowIdx, chars, lineParser) match {
					case Complex(nextFolding) => Recurse(nextFolding)
					case BuilderFailure(bf) => ReturnFailure(bf)
					case ParserRetVal.ParserFailure(pf) => pf:Nothing
					case ParserRetVal.Primitive(x) => x:Nothing
				}
			} catch {
				// Readers don't have a hasNext
				case ex:java.util.NoSuchElementException => ReturnFolding
			}
			
			thingToDo match {
				case Recurse(nextFolding) => dothing(rowIdx + 1, nextFolding)
				case ReturnFolding => Complex(folding)
				case ReturnFailure(x) => BuilderFailure(x)
			}
		}
		
		dothing(0, builder.init)
			.complex.flatMap{builder.finalize _}
	}
}

/**
 * Contains classes used to customize the CsvParser's behavior, as
 * well as a few common instances of those classes.
 * @since 2.0
 * @version 3.0.1
 */
object CsvParser {
	/**
	 * A data container which tells the CsvParser which characters are special
	 * @since 2.0
	 * @version 2.0
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
	 * @since 2.0
	 * @version 2.0
	 */
	val csvCharacterMeanings = CharacterMeanings(Set('\n'), Set(','), Set('"'), Set(' ', '\t', '\uFEFF'), Set('\\'))
	
	/**
	 * A CharacterMeanings that uses a set of characters similar to most Tab-Separated-Values files
	 * @since 2.0
	 * @version 2.0
	 */
	val tsvCharacterMeanings = CharacterMeanings(Set('\n'), Set('\t'), Set('"'), Set(' ', '\uFEFF'), Set('\\'))
	
	/**
	 * A CharacterMeanings that uses ASCII record and field delimiter characters to separate records and fields
	 * @since 2.0
	 * @version 2.0
	 */
	val asciiCharacterMeanings = CharacterMeanings(Set('\u001E'), Set('\u001F'), Set.empty, Set.empty, Set.empty)
	
	
	
	
	/**
	 * @param value the current value. Either a left describing an error, or a right with a complex value
	 * @param innerIndex the current character index of the parse
	 * @param innerInput the 
	 * @param endingWhitespace Whitespace that should be ignored if a delimiter is encountered
	 * 		before another non-whitespace character, but that should not be ignored otherwise.
	 * @param quoted true if the parser is currently in a quoted state. characters between quote characters
	 * 		are treated literally, so if this is true then characters should be treated literally,
	 * 		and this should be toggled upon encountering a quote character.
	 * @param escaped true if the parser is currently in an escape state. Characters after an escape character
	 * 		is treated literally, so if this is true then characters should be treated literally,
	 *		become true when an escape character is encountered and become false after encountering that literal character
	 */
	private[parser] final case class State[A, BF] (
		value:ParserRetVal[A, Nothing, Nothing, Indexed[BF]],
		fieldStartIndex:Int,
		innerIndex:Int,
		innerInput:String,
		endingWhitespace:String,
		quoted:Boolean,
		escaped:Boolean
	) {
		def appendChar(c:Char):State[A,BF] = this.copy(endingWhitespace = "", innerInput = innerInput + endingWhitespace + c)
	}
	
	
	/** Splits a CSV record (i.e. one line) into fields */
	private[parser] final class LineParser(meaningfulCharacters:CsvParser.CharacterMeanings) extends Parser[Int, String, Nothing, CountingReader] {
		def parse[Result, BF](builder:Builder[Int, String, BF, Result], chars:CountingReader):ParserRetVal[Result, Nothing, Nothing, BF] = {
			val rowStartIndex = chars.index
			var state = State[builder.Middle, BF](Complex(builder.init), rowStartIndex, 0, "", "", false, false)
			try {
				var char = chars.read()
				
				while (!(meaningfulCharacters.recordDelimeter contains char)) {
					state = if (state.escaped) {
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
							value = state.value
									.complex.flatMap{x => builder.apply(x, state.innerIndex, state.innerInput, new IdentityParser[String])
										.builderFailure.map{msg => Indexed(msg, rowStartIndex + state.fieldStartIndex)}
									},
							fieldStartIndex = chars.index,
							innerIndex = state.innerIndex + 1,
							innerInput = "",
							endingWhitespace = "",
							quoted = false,
							escaped = false
						)
					} else {
						state.appendChar(char)
					}
					
					char = chars.read()
				}
			} catch {
				case ex:java.util.NoSuchElementException => {
					// Readers don't have a hasNext
				}
			}
			
			(
				if (state.innerInput.isEmpty) {
					state.value
				} else {
					state.value.complex.flatMap{x =>
						builder.apply(x, state.innerIndex, state.innerInput, new IdentityParser[String])
								.builderFailure.map{msg => Indexed(msg, rowStartIndex + state.fieldStartIndex)}
					}
				}
			)
			// TODO: annotations
			.builderFailure.map{_.cause}
			.complex.flatMap{builder.finalize _}
		}
	}
}
