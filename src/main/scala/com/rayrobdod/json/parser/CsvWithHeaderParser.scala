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
import com.rayrobdod.json.builder.{Builder, PrimitiveSeqBuilder}
import com.rayrobdod.json.union.{StringOrInt, ParserRetVal}
import com.rayrobdod.json.union.ParserRetVal.{Complex, BuilderFailure, ParserFailure}

/**
 * A streaming decoder for csv data, where the first line of the csv data is a header row.
 * 
 * This parser is lenient, in that it ignores trailing delimiters
 * 
 * A CSV file is always two levels deep, an array of key-value mappings.
 * @version 4.0
 * 
 * @constructor
 * Creates a CsvParser instance.
 * @param meaningfulCharacters indicates which characters have special meanings
 */
// TODO: location annotation
final class CsvWithHeaderParser(
		meaningfulCharacters:CsvParser.CharacterMeanings = CsvParser.csvCharacterMeanings
) extends Parser[StringOrInt, String, Nothing, Iterable[Char]] {
	private[this] val lineParser = new CsvParser.LineParser(meaningfulCharacters)
	
	/**
	 * Decodes the input values to an object.
	 * @tparam Result the type of object to create
	 * @param builder the builder to use to construct the object
	 * @param chars the serialized json object or array
	 * @return the parsed object
	 */
	def parse[Result, BF](builder:Builder[StringOrInt, String, BF, Result], chars:Iterable[Char]):ParserRetVal[Result,Nothing, Nothing, BF] = {
		this.parse(builder, new Iterator2Reader(chars.iterator))
	}
	
	/**
	 * Decodes the input values to an object.
	 * @tparam Result the type of object to create
	 * @param builder the builder to use to construct the object
	 * @param chars the serialized json object or array
	 * @return the parsed object
	 */
	 def parse[Result, BF](builder:Builder[StringOrInt, String, BF, Result], chars:java.io.Reader):ParserRetVal[Result,Nothing, Nothing, BF] = {
		this.parse(builder, new CountingReader(chars))
	}
	
	def parse[Result, BF](builder:Builder[StringOrInt, String, BF, Result], chars:CountingReader):ParserRetVal[Result,Nothing, Nothing, BF] = {
		val keys:Seq[String] = lineParser.parse(new PrimitiveSeqBuilder[String], chars).fold({x => x}, {x => x:Nothing}, {x => x:Nothing}, {x => Nil})
		val myLineParser = lineParser.mapKey[StringOrInt]{i =>
			if (keys.isDefinedAt(i)) {StringOrInt(keys(i))} else {StringOrInt(i)}
		}
		
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
				
				builder.apply(folding, StringOrInt(rowIdx), chars, myLineParser) match {
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
