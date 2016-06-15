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
package com.rayrobdod.json.parser

import scala.util.{Either, Left, Right}
import com.rayrobdod.json.builder.Builder
import com.rayrobdod.json.union.ParserRetVal

/**
 * A parser
 * @since next
 * @tparam Key the key types
 * @tparam Value the primitive value types
 * @tparam Input the input to the parser
 */
trait Parser[Key, Value, Input] {
	
	/**
	 * Parse the input into either a Value or an Output
	 * @param builder a builder in the case the the parser finds a complex value
	 * @param i the input to the parser
	 * @tparam ComplexOutput the type of object the Builder produces
	 */
	def parse[ComplexOutput](builder:Builder[Key, Value, ComplexOutput], i:Input):ParserRetVal[ComplexOutput, Value]
	
	
	final def parsePrimitive(i:Input):Either[(String, Int), Value] = {
		val ignoreAllBuilder = new Builder[Key, Value, Any] {
			def init:Any = this
			def apply[I](a:Any,k:Key,i:I,p:Parser[Key,Value,I]):Either[(String, Int), Any] = Right(a)
		}
		
		this.parse(ignoreAllBuilder, i).fold({c => Left(("Expected primitive value", 0))}, {p => Right(p)}, {(m,i) => Left((m,i))})
	}
	
	
	/** Change the type of key that this builder requires */
	final def mapKey[K2](implicit fun:Function1[Key,K2]):Parser[K2,Value,Input] = new Parser[K2,Value,Input] {
		override def parse[Output](builder:Builder[K2,Value,Output], i:Input):ParserRetVal[Output, Value] = {
			Parser.this.parse(builder.mapKey[Key](fun), i)
		}
	}
	
	/** Change the type of value that this builder requires */
	final def mapValue[V2](implicit fun:Function1[Value,V2]):Parser[Key,V2,Input] = new Parser[Key,V2,Input] {
		override def parse[Output](builder:Builder[Key,V2,Output], i:Input):ParserRetVal[Output, V2] = {
			import ParserRetVal._
			Parser.this.parse[Output](builder.mapValue[Value](fun), i).fold({x => Complex(x)}, {x => Primitive(fun(x))}, {(m,i) => Failure(m,i)})
		}
	}
	
}
