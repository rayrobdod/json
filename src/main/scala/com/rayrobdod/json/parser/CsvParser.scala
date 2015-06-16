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
 * == Primitive types ==
 - java.lang.String
 * 
 * @constructor
 * Creates a CsvParser instance.
 * @param topBuilder the builder that this parser will use when constructing objects
 * @param meaningfulCharacters determines which characters have special meanings
 */
final class CsvParser[A](topBuilder:Builder[A], meaningfulCharacters:CsvParser.CharacterMeanings = CsvParser.csvCharacterMeanings) {
	
	/**
	 * Decodes the input values to an object.
	 * @param chars the serialized json object or array
	 * @return the parsed object
	 */
	def parse(chars:Iterable[Char]):A = {
		chars.zipWithIndex.foldLeft[State](new StartOfRecordState()){
			(state:State, charIndex:(Char, Int)) => state.apply(charIndex._1, charIndex._2)
		}.topValue
	}
	
	/**
	 * Decodes the input values to an object.
	 * @param chars the serialized json object or array
	 * @return the parsed object
	 */
	def parse(chars:java.io.Reader):A = this.parse(new Reader2Iterable(chars))
	
	
	private trait State {
		def topValue:A
		def apply(c:Char, index:Int):State
	}
	
	private case class StartOfRecordState(
			topValue:A = topBuilder.init,
			topIndex:Int = 0
	) extends State {
		override def apply(c:Char, index:Int):State = {
			if (meaningfulCharacters.ignorable.contains(c)) {
				this
			} else if (meaningfulCharacters.stringDelimeter.contains(c)) {
				new QuotedState(
					topValue,
					topIndex,
					topBuilder.childBuilder(topIndex.toString).init,
					0,
					""
				)
			} else {
				new NormalState(
					topValue,
					topIndex,
					topBuilder.childBuilder(topIndex.toString).init,
					0,
					""
				).apply(c, index)
			}
		}
	}
	
	private case class StartOfFieldState(
			topVal:A,
			topIndex:Int,
			innerValue:Any,
			innerIndex:Int
	) extends State {
		override def topValue:A = {
			topBuilder.apply(topVal, topIndex.toString, innerValue)
		}
		
		override def apply(c:Char, index:Int):State = {
			if (meaningfulCharacters.ignorable.contains(c)) {
				this
			} else if (meaningfulCharacters.recordDelimeter.contains(c)) {
				StartOfRecordState(
					this.topValue,
					topIndex + 1
				)
			} else if (meaningfulCharacters.stringDelimeter.contains(c)) {
				new QuotedState(
					topVal,
					topIndex,
					innerValue,
					innerIndex,
					""
				)
			} else {
				new NormalState(
					topVal,
					topIndex,
					innerValue,
					innerIndex,
					""
				).apply(c, index)
			}
		}
	}
	
	private case class NormalState(
			topVal:A,
			topIndex:Int,
			innerValue:Any,
			innerIndex:Int,
			val string:String
	) extends State {
		private val childBuilder = topBuilder.childBuilder(topIndex.toString).asInstanceOf[Builder[Any]]
		override def topValue:A = {
			val newInnerValue = childBuilder.apply(innerValue, innerIndex.toString, string)
			topBuilder.apply(topVal, topIndex.toString, newInnerValue)
		}
		
		override def apply(c:Char, index:Int):State = {
			if (meaningfulCharacters.recordDelimeter.contains(c)) {
				StartOfRecordState(
					this.topValue,
					topIndex + 1
				)
			} else if (meaningfulCharacters.fieldDelimeter.contains(c)) {
				StartOfFieldState(
					topVal,
					topIndex,
					childBuilder.apply(innerValue, innerIndex.toString, string),
					innerIndex + 1
				)
			} else if (meaningfulCharacters.ignorable.contains(c)) {
				EndingWhitespaceState(
					topVal,
					topIndex,
					innerValue,
					innerIndex,
					string,
					"" + c
				)
			} else if (meaningfulCharacters.escape.contains(c)) {
				new EscapeState(this)
			} else {
				this.copy(string = string + c)
			}
		}
	}
	
	private case class EndingWhitespaceState(
			topVal:A,
			topIndex:Int,
			innerValue:Any,
			innerIndex:Int,
			string:String,
			endingWhitespace:String
	) extends State {
		private val childBuilder = topBuilder.childBuilder(topIndex.toString).asInstanceOf[Builder[Any]]
		
		override def topValue:A = {
			val newInnerValue = childBuilder.apply(innerValue, innerIndex.toString, string)
			topBuilder.apply(topVal, topIndex.toString, newInnerValue)
		}
		
		override def apply(c:Char, index:Int):State = {
			if (meaningfulCharacters.recordDelimeter.contains(c)) {
				StartOfRecordState(
					this.topValue,
					topIndex + 1
				)
			} else if (meaningfulCharacters.fieldDelimeter.contains(c)) {
				StartOfFieldState(
					topVal,
					topIndex,
					childBuilder.apply(innerValue, innerIndex.toString, string),
					innerIndex + 1
				)
			} else if (meaningfulCharacters.ignorable.contains(c)) {
				this.copy(endingWhitespace = endingWhitespace + c)
			} else {
				NormalState(
					topVal,
					topIndex,
					innerValue,
					innerIndex,
					string + endingWhitespace
				).apply(c, index)
			}
		}
	}
	
	private case class QuotedState(
			val topVal:A,
			topIndex:Int,
			innerValue:Any,
			innerIndex:Int,
			string:String
	) extends State {
		private val correspondingNormalState = new NormalState(topVal, topIndex, innerValue, innerIndex, string)
		
		override def topValue:A = correspondingNormalState.topValue
		
		override def apply(c:Char, index:Int):State = {
			if (meaningfulCharacters.stringDelimeter.contains(c)) {
				correspondingNormalState
			} else {
				this.copy(string = this.string + c)
			}
		}
	}
	
	
	private case class EscapeState(
			correspondingNormalState:NormalState
	) extends State {

		override def topValue:A = correspondingNormalState.topValue
		override def apply(c:Char, i:Int):State = {
			correspondingNormalState.copy(string = correspondingNormalState.string + c)
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

