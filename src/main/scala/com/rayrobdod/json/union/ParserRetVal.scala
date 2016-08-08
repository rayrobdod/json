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
package com.rayrobdod.json.union

import com.rayrobdod.json.builder.Builder
import com.rayrobdod.json.parser.Parser
import scala.language.implicitConversions

/*
 * The other classes in this package could easily be unions if the language supported anonymous unions.
 * ParserRetVal, on the other hand, probably needs to be a tagged union.
 * The Complex output implies a level of processing which the Primitive almost certainly doesn't have,
 * and it is conceivable that the complex output is a (String, Int) tuple, which would overlap with the Failure type.
 */

/**
 * A union type representing possible return values of [[com.rayrobdod.json.parser.Parser.parse `Parser.parse`]].
 * 
 * @since 3.0
 */
sealed trait ParserRetVal[+Complex, +Primitive]{
	/**
	 * Applies a function corresponding to `this`'s type
	 * @param c the function to apply if `this` is a [[ParserRetVal$.Complex Complex]]
	 * @param p the function to apply if `this` is a [[ParserRetVal$.Primitive Primitive]]
	 * @param f the function to apply if `this` is a [[ParserRetVal$.Failure Failure]]
	 * @return the results of applying the function
	 */
	def fold[Out](c:Function1[Complex,Out], p:Function1[Primitive,Out], f:Function2[String,Int,Out]):Out
}

/**
 * A container for the types of [ParserRetVal]s
 * @since 3.0
 */
object ParserRetVal {
	
	final case class Primitive[Primitive](x:Primitive) extends ParserRetVal[Nothing, Primitive]{
		def fold[Out](c:Function1[Nothing,Out], p:Function1[Primitive,Out], f:Function2[String,Int,Out]):Out = p(x)
	}
	
	final case class Complex[Complex](x:Complex) extends ParserRetVal[Complex, Nothing]{
		def fold[Out](c:Function1[Complex,Out], p:Function1[Nothing,Out], f:Function2[String,Int,Out]):Out = c(x)
	}
	
	/**
	 * @constructor
	 * @param msg a string describing the failure
	 * @param idx the location in the input of the error. The meaning of idx depends on the Parser's Input;
	 * 	if the Input is a character sequence, then idx might be the index of the character that caused a problem.
	 */
	final case class Failure(msg:String, idx:Int) extends ParserRetVal[Nothing, Nothing]{
		def fold[Out](c:Function1[Nothing,Out], p:Function1[Nothing,Out], f:Function2[String,Int,Out]):Out = f(msg,idx)
	}
	
}
