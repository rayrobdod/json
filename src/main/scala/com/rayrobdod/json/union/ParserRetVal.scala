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
import com.rayrobdod.json.union.ParserRetVal.{ComplexProjection, PrimitiveProjection, FailureProjection}

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
 * @version next
 */
sealed trait ParserRetVal[+Complex, +Primitive] {
	/**
	 * Applies a function corresponding to `this`'s type
	 * @param c the function to apply if `this` is a [[ParserRetVal$.Complex Complex]]
	 * @param p the function to apply if `this` is a [[ParserRetVal$.Primitive Primitive]]
	 * @param f the function to apply if `this` is a [[ParserRetVal$.Failure Failure]]
	 * @return the results of applying the function
	 */
	def fold[Out](c:Function1[Complex,Out], p:Function1[Primitive,Out], f:Function2[String,Int,Out]):Out
	
	/** projects this ParserRetVal as a primitive */
	def primitive:PrimitiveProjection[Complex,Primitive] = new PrimitiveProjection(this)
	/** projects this ParserRetVal as a complex */
	def complex:ComplexProjection[Complex,Primitive] = new ComplexProjection(this)
	/** projects this ParserRetVal as a failure
	 * @since next
	 */
	def failure:FailureProjection[Complex,Primitive] = new FailureProjection(this)
	
	/** Convert a Failure into a Left and the other two cases into a Right  */
	def mergeToEither[A](implicit ev1:Primitive <:< A, ev2:Complex <:< A):Either[(String,Int),A] = {
		this.fold({c => Right(ev2(c))}, {p => Right(ev1(p))}, {(s,i) => Left(s,i)})
	}
	def mergeToComplex[A](implicit ev1:Primitive <:< A, ev2:Complex <:< A):NonPrimitiveParserRetVal[A] = {
		this.fold({c => ParserRetVal.Complex(ev2(c))}, {p => ParserRetVal.Complex(ev1(p))}, {(s,i) => ParserRetVal.Failure(s,i)})
	}
}

/**
 * A container for the types of [[ParserRetVal]]s
 * @since 3.0
 * @version 3.0.1
 */
object ParserRetVal {
	
	/** Convert a Left to a Failure and a Right to a Complex */
	def eitherToComplex[A](a:Either[(String,Int),A]):ParserRetVal[A,Nothing] = {
		a.fold({case (msg,idx) => ParserRetVal.Failure(msg,idx)}, {x => ParserRetVal.Complex(x)})
	}
	/** Convert a Left to a Failure and a Right to a Primitive */
	def eitherToPrimitive[A](a:Either[(String,Int),A]):ParserRetVal[Nothing,A] = {
		a.fold({case (msg,idx) => ParserRetVal.Failure(msg,idx)}, {x => ParserRetVal.Primitive(x)})
	}
	
	/** Represents a value that was produced by the parser without builder consultation */
	final case class Primitive[+P](x:P) extends ParserRetVal[Nothing, P]{
		def fold[Out](c:Function1[Nothing,Out], p:Function1[P,Out], f:Function2[String,Int,Out]):Out = p(x)
	}
	
	/** Represents a value that was produced via consultation of a builder */
	final case class Complex[+C](x:C) extends ParserRetVal[C, Nothing] with NonPrimitiveParserRetVal[C] {
		def fold[Out](c:Function1[C,Out], p:Function1[Nothing,Out], f:Function2[String,Int,Out]):Out = c(x)
	}
	
	/**
	 * Represents an attempt at parsing that was unsuccessful
	 * @constructor
	 * @param msg a string describing the failure
	 * @param idx the location in the input of the error. The meaning of idx depends on the Parser's Input;
	 * 	if the Input is a character sequence, then idx might be the index of the character that caused a problem.
	 */
	final case class Failure(msg:String, idx:Int) extends ParserRetVal[Nothing, Nothing] with NonPrimitiveParserRetVal[Nothing] {
		def fold[Out](c:Function1[Nothing,Out], p:Function1[Nothing,Out], f:Function2[String,Int,Out]):Out = f(msg,idx)
	}
	
	
	/** A projection as if the ParserRetVal were a primitive */
	final class PrimitiveProjection[+C,+P](backing:ParserRetVal[C,P]) {
		
		/** Map the backing value if the backing value is a Primitive, else return the backing value */
		def map[X](fun:P => X):ParserRetVal[C,X] = backing match {
			case Primitive(p) => Primitive(fun(p))
			case c:Complex[C] => c
			case f:Failure => f
		}
		
		/** Flatmap the backing value if the backing value is a Primitive, else return the backing value */
		def flatMap[CC >: C, X](fun:P => ParserRetVal[CC, X]):ParserRetVal[CC, X] = backing match {
			case Primitive(p) => fun(p)
			case c:Complex[C] => c
			case f:Failure => f
		}
		
		/** Return a Right if the backing value is a Primitive, else return a left */
		def toEither:Either[(String,Int),P] = backing match {
			case Primitive(p) => Right(p)
			case c:Complex[C] => Left(("Expected primitive value", 0))
			case Failure(msg, idx) => Left((msg, idx))
		}
	}
	
	/** A projection as if the ParserRetVal were a Complex */
	sealed class ComplexProjection[+C,+P](backing:ParserRetVal[C,P]) {
		
		/** Map the backing value if the backing value is a Complex, else return the backing value */
		def map[X](fun:C => X):ParserRetVal[X,P] = backing match {
			case Complex(c) => Complex(fun(c))
			case p:Primitive[P] => p
			case f:Failure => f
		}
		
		/** Flatmap the backing value if the backing value is a Complex, else return the backing value
		 * @since 3.1
		 */
		def flatMap[PP >: P, X](fun:C => ParserRetVal[X, PP]):ParserRetVal[X, PP] = backing match {
			case Complex(c) => fun(c)
			case p:Primitive[P] => p
			case f:Failure => f
		}
		
		/** Return a Right if the backing value is a Complex, else return a left */
		def toEither:Either[(String,Int),C] = backing match {
			case Complex(c) => Right(c)
			case p:Primitive[P] => Left(("Expected complex value", 0))
			case Failure(msg, idx) => Left((msg, idx))
		}
	}
	
	/** A projection as if the ParserRetVal were a Failure
	 * @since next
	 */
	sealed class FailureProjection[+C,+P](backing:ParserRetVal[C,P]) {
		
		/** Map the backing value if the backing value is a Failure, else return the backing value */
		def map(fun:(String, Int) => (String, Int)):ParserRetVal[C,P] = backing match {
			case c:Complex[C] => c
			case p:Primitive[P] => p
			case Failure(msg, idx) => {
				val (newMsg, newIdx) = fun.apply(msg, idx)
				Failure(newMsg, newIdx)
			}
		}
	}
	
}

/** 
 * @since next
 */
sealed trait NonPrimitiveParserRetVal[+Success] extends ParserRetVal[Success, Nothing] {
	import ParserRetVal.{Complex, Failure}
	
	def fold[Out](fc:Function1[Success,Out], ff:Function2[String,Int,Out]):Out = this match {
		case Complex(c) => fc(c)
		case Failure(m,i) => ff(m,i)
	}
	
	/** projects this ParserRetVal as a complex */
	override def complex:NonPrimitiveParserRetVal.ComplexProjection[Success] = new NonPrimitiveParserRetVal.ComplexProjection(this)
	/** projects this ParserRetVal as a failure */
	override def failure:NonPrimitiveParserRetVal.FailureProjection[Success] = new NonPrimitiveParserRetVal.FailureProjection(this)
}

/** 
 * @since next
 */
object NonPrimitiveParserRetVal {
	import ParserRetVal.{Complex, Failure}
	
	/** A projection as if the ParserRetVal were a Complex */
	final class ComplexProjection[+C](backing:NonPrimitiveParserRetVal[C]) extends ParserRetVal.ComplexProjection(backing) {
		
		/** Map the backing value if the backing value is a Complex, else return the backing value */
		override def map[X](fun:C => X):NonPrimitiveParserRetVal[X] = backing match {
			case Complex(c) => Complex(fun(c))
			case f:Failure => f
		}
		
		/** Flatmap the backing value if the backing value is a Complex, else return the backing value
		 */
		def flatMap[X](fun:C => NonPrimitiveParserRetVal[X]):NonPrimitiveParserRetVal[X] = backing match {
			case Complex(c) => fun(c)
			case f:Failure => f
		}
	}
	
	/** A projection as if the ParserRetVal were a Complex */
	final class FailureProjection[+C](backing:NonPrimitiveParserRetVal[C]) extends ParserRetVal.FailureProjection(backing) {
		
		/** Map the backing value if the backing value is a Failure, else return the backing value */
		override def map(fun:(String, Int) => (String, Int)):NonPrimitiveParserRetVal[C] = backing match {
			case c:Complex[C] => c
			case Failure(msg, idx) => {
				val (newMsg, newIdx) = fun.apply(msg, idx)
				Failure(newMsg, newIdx)
			}
		}
	}
}
