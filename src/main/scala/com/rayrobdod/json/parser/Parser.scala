package com.rayrobdod.json.parser

import com.rayrobdod.json.builder.Builder

/**
 * A parser
 * @tparam Key the key types
 * @tparam Value the primitive value types
 * @tparam Input the input to the parser
 */
trait Parser[Key, Value, Input] {
	// NOTE: Either[Throwable, Output] instead of throwing stuff?
	/**
	 * Parses the input into a complex value
	 */
	@deprecated("asdf", "next")
	def parseComplex[Output](builder:Builder[Key, Value, Output], i:Input):Output
	
	/**
	 * Parses the input into a primitive value
	 */
	@deprecated("use parseEither instead", "next")
	def parsePrimitive(i:Input):Value
	
	/**
	 * Parse the input into either a Value or an Output
	 */
	def parseEither[ComplexOutput](builder:Builder[Key, Value, ComplexOutput], i:Input):Either[ComplexOutput, Value]
}
