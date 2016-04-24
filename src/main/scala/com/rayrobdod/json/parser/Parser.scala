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
	 * Parse the input into either a Value or an Output
	 * @param builder a builder in the case the the parser finds a complex value
	 * @param i the input to the parser
	 * @tparam ComplexOutput the type of object the Builder produces
	 */
	def parse[ComplexOutput](builder:Builder[Key, Value, ComplexOutput], i:Input):Either[ComplexOutput, Value]
}
