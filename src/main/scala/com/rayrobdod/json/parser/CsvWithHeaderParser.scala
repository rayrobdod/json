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
 * @version next
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
		this.parse(builder, new Iterator2Reader(chars.iterator))
	}
	
	/**
	 * Decodes the input values to an object.
	 * @tparam A the type of object to create
	 * @param builder the builder to use to construct the object
	 * @param chars the serialized json object or array
	 * @return the parsed object
	 */
	 def parse[A](builder:Builder[StringOrInt, String, A], chars:java.io.Reader):ParserRetVal[A,Nothing] = {
		this.parse(builder, new CountingReader(chars))
	}
	
	def parse[A](builder:Builder[StringOrInt, String, A], chars:CountingReader):ParserRetVal[A,Nothing] = {
		var folding:Either[(String, Int), A] = Right(builder.init)
		var rowIdx:Int = 0
		
		val keys:Seq[String] = lineParser.parse(new PrimitiveSeqBuilder[String], chars).fold({x => x}, {x:Nothing => x}, {(s,i) => Nil})
		val myLineParser = lineParser.mapKey[StringOrInt]{i =>
			if (keys.isDefinedAt(i)) {StringOrInt(keys(i))} else {StringOrInt(i)}
		}
		
		try {
			while (folding.isRight) {
				// check that there are more characters to read; throw if no such characters
				// I have to do this as readers don't have a hasNext or similar method
				chars.read()
				chars.goBackOne()
				val rowStartCharIndex = chars.index + 1
				
				folding = builder.apply(folding.right.get, StringOrInt(rowIdx), chars, myLineParser).left.map{x => ((x._1, x._2 + rowStartCharIndex))}
				rowIdx = rowIdx + 1
			}
		} catch {
			case ex:java.util.NoSuchElementException => {
				// Readers don't have a hasNext
			}
		}
		
		ParserRetVal.eitherToComplex(folding)
	}
}
