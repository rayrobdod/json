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
 * @version 4.0
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
	 * @since 4.0
	 */
	def failure:FailureProjection[Complex,Primitive] = new FailureProjection(this)
	
	def mergeToComplex[A](implicit ev1:Primitive <:< A, ev2:Complex <:< A):ParserRetVal[A,Nothing] = {
		this.fold({c => ParserRetVal.Complex(ev2(c))}, {p => ParserRetVal.Complex(ev1(p))}, {(s,i) => ParserRetVal.Failure(s,i)})
	}	/**
	 * @since 4.0
	 */
	def flip:ParserRetVal[Primitive, Complex] = this match {
		case ParserRetVal.Complex(x) => ParserRetVal.Primitive(x)
		case ParserRetVal.Primitive(x) => ParserRetVal.Complex(x)
		case x:ParserRetVal.Failure => x
	}
	
	/**
	 * Map Complex to a new type. Only avaliable if Primitive is Nothing
	 * @since 4.0
	 */
	def map[A](fun : Complex => A)(implicit ev:Primitive <:< Nothing):ParserRetVal[A,Primitive] = this match {
		case ParserRetVal.Complex(x) => ParserRetVal.Complex(fun(x))
		case ParserRetVal.Primitive(x) => ParserRetVal.Primitive(x)
		case x:ParserRetVal.Failure => x
	}
	
	/**
	 * Map Primtive to a new type. Only avaliable if Complex is Nothing
	 * @since 4.0
	 */
	def map[A](fun : Primitive => A)(implicit ev:Complex <:< Nothing, ev2: Int =:= Int):ParserRetVal[Complex,A] = this match {
		case ParserRetVal.Complex(x) => ParserRetVal.Complex(x)
		case ParserRetVal.Primitive(x) => ParserRetVal.Primitive(fun(x))
		case x:ParserRetVal.Failure => x
	}
	
	/**
	 * Flatmap Complex to a new type. Only avaliable if Primitive is Nothing
	 * @since 4.0
	 */
	def flatMap[XX, PP](fun : Complex => ParserRetVal[XX, PP])(implicit ev:Primitive <:< Nothing):ParserRetVal[XX, PP] = this match {
		case ParserRetVal.Complex(x) => fun(x)
		case ParserRetVal.Primitive(x) => ParserRetVal.Primitive(ev(x))
		case x:ParserRetVal.Failure => x
	}
	
	/**
	 * Flatmap Primtive to a new type. Only avaliable if Complex is Nothing
	 * @since 4.0
	 */
	def flatMap[CC, XX](fun : Primitive => ParserRetVal[CC, XX])(implicit ev:Complex <:< Nothing, ev2: Int =:= Int):ParserRetVal[CC, XX] = this match {
		case ParserRetVal.Complex(x) => ParserRetVal.Complex(ev(x))
		case ParserRetVal.Primitive(x) => fun(x)
		case x:ParserRetVal.Failure => x
	}
}

/**
 * A container for the types of [[ParserRetVal]]s
 * @since 3.0
 * @version 4.0
 */
object ParserRetVal {
	
	/** Represents a value that was produced by the parser without builder consultation */
	final case class Primitive[+P](x:P) extends ParserRetVal[Nothing, P]{
		def fold[Out](c:Function1[Nothing,Out], p:Function1[P,Out], f:Function2[String,Int,Out]):Out = p(x)
	}
	
	/** Represents a value that was produced via consultation of a builder */
	final case class Complex[+C](x:C) extends ParserRetVal[C, Nothing] {
		def fold[Out](c:Function1[C,Out], p:Function1[Nothing,Out], f:Function2[String,Int,Out]):Out = c(x)
	}
	
	/**
	 * Represents an attempt at parsing that was unsuccessful
	 * @constructor
	 * @param msg a string describing the failure
	 * @param idx the location in the input of the error. The meaning of idx depends on the Parser's Input;
	 * 	if the Input is a character sequence, then idx might be the index of the character that caused a problem.
	 */
	final case class Failure(msg:String, idx:Int) extends ParserRetVal[Nothing, Nothing] {
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
