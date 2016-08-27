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

import scala.util.{Either, Left, Right}
import com.rayrobdod.json.union.ParserRetVal.{ComplexProjection, PrimitiveProjection}
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
	
	/** projects this ParserRetVal as a primitive */
	def primitive:PrimitiveProjection[Complex,Primitive]
	/** projects this ParserRetVal as a complex */
	def complex:ComplexProjection[Complex,Primitive]
	
	/** Convert a Failure into a Left and the other two options into a Right  */
	def mergeToEither[A](implicit ev1:Primitive <:< A, ev2:Complex <:< A):Either[(String,Int),A] = this.fold({c => Right(ev2(c))}, {p => Right(ev1(p))}, {(s,i) => Left(s,i)})
}

/**
 * A container for the types of [ParserRetVal]s
 * @since 3.0
 */
object ParserRetVal {
	
	def eitherToComplex[A](a:Either[(String,Int),A]):ParserRetVal[A,Nothing] = {
		a.fold({case (msg,idx) => ParserRetVal.Failure(msg,idx)}, {x => ParserRetVal.Complex(x)})
	}
	
	final case class Primitive[Primitive](x:Primitive) extends ParserRetVal[Nothing, Primitive]{
		def fold[Out](c:Function1[Nothing,Out], p:Function1[Primitive,Out], f:Function2[String,Int,Out]):Out = p(x)
		def primitive:PrimitiveProjection[Nothing,Primitive] = new PrimitiveProjection[Nothing,Primitive]{
			def map[C](fun:Primitive => C):ParserRetVal.Primitive[C] = Primitive(fun(x))
			def toEither:Either[(String,Int),Primitive] = Right(x)
		}
		def complex:ComplexProjection[Nothing,Primitive] = new ComplexProjection[Nothing,Primitive]{
			def map[C](fun:Nothing => C):ParserRetVal.Primitive[Primitive] = Primitive.this
			def toEither:Either[(String,Int),Nothing] = Left("Expected complex value", 0)
		}
	}
	
	final case class Complex[Complex](x:Complex) extends ParserRetVal[Complex, Nothing]{
		def fold[Out](c:Function1[Complex,Out], p:Function1[Nothing,Out], f:Function2[String,Int,Out]):Out = c(x)
		def primitive:PrimitiveProjection[Complex,Nothing] = new PrimitiveProjection[Complex,Nothing]{
			def map[C](fun:Nothing => C):ParserRetVal.Complex[Complex] = Complex.this
			def toEither:Either[(String,Int),Nothing] = Left("Expected primitive value", 0)
		}
		def complex:ComplexProjection[Complex,Nothing] = new ComplexProjection[Complex,Nothing]{
			def map[C](fun:Complex => C):ParserRetVal.Complex[C] = Complex(fun(x))
			def toEither:Either[(String,Int),Complex] = Right(x)
		}
	}
	
	/**
	 * @constructor
	 * @param msg a string describing the failure
	 * @param idx the location in the input of the error. The meaning of idx depends on the Parser's Input;
	 * 	if the Input is a character sequence, then idx might be the index of the character that caused a problem.
	 */
	final case class Failure(msg:String, idx:Int) extends ParserRetVal[Nothing, Nothing]{
		def fold[Out](c:Function1[Nothing,Out], p:Function1[Nothing,Out], f:Function2[String,Int,Out]):Out = f(msg,idx)
		
		private[this] object biased extends PrimitiveProjection[Nothing, Nothing] with ComplexProjection[Nothing, Nothing] {
			def map[C](fun:Nothing => C):ParserRetVal.Failure = Failure.this
			def toEither:Either[(String,Int),Nothing] = Left((msg, idx))
		}
		def primitive:PrimitiveProjection[Nothing,Nothing] = biased
		def complex:ComplexProjection[Nothing,Nothing] = biased
	}
	
	
	sealed trait PrimitiveProjection[+A,+B] {
		/** Map the backing value if the backing value is a Primitive, else return the backing value */
		def map[C](fun:B => C):ParserRetVal[A,C]
		/** Return a Right if the backing value is a Primitive, else return a left */
		def toEither:Either[(String,Int),B]
	}
	sealed trait ComplexProjection[+A,+B] {
		/** Map the backing value if the backing value is a Complex, else return the backing value */
		def map[C](fun:A => C):ParserRetVal[C,B]
		/** Return a Right if the backing value is a Complex, else return a left */
		def toEither:Either[(String,Int),A]
	}
	
}
