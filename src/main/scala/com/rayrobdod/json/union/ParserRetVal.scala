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

import com.rayrobdod.json.union.ParserRetVal.{ComplexProjection, PrimitiveProjection, ParserFailureProjection, BuilderFailureProjection}

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
sealed trait ParserRetVal[+Complex, +Primitive, +ParserFailure, +BuilderFailure] {
	/**
	 * Applies a function corresponding to `this`'s type
	 * @version 4.0
	 * @param c the function to apply if `this` is a [[ParserRetVal$.Complex Complex]]
	 * @param p the function to apply if `this` is a [[ParserRetVal$.Primitive Primitive]]
	 * @param f the function to apply if `this` is a [[ParserRetVal$.ParserFailure ParserFailure]]
	 * @param f the function to apply if `this` is a [[ParserRetVal$.BuilderFailure BuilderFailure]]
	 * @return the results of applying the function
	 */
	def fold[Out](c:Function1[Complex,Out], p:Function1[Primitive,Out], f:Function1[ParserFailure,Out], b:Function1[BuilderFailure,Out]):Out
	
	/** projects this ParserRetVal as a primitive
	 * @version 4.0
	 */
	def primitive:PrimitiveProjection[Complex,Primitive,ParserFailure,BuilderFailure] = new PrimitiveProjection(this)
	/** projects this ParserRetVal as a complex
	 * @version 4.0
	 */
	def complex:ComplexProjection[Complex,Primitive,ParserFailure,BuilderFailure] = new ComplexProjection(this)
	/** projects this ParserRetVal as a parserFailure
	 * @since 4.0
	 */
	def parserFailure:ParserFailureProjection[Complex,Primitive,ParserFailure,BuilderFailure] = new ParserFailureProjection(this)
	/** projects this ParserRetVal as a parserFailure
	 * @since 4.0
	 */
	def builderFailure:BuilderFailureProjection[Complex,Primitive,ParserFailure,BuilderFailure] = new BuilderFailureProjection(this)
	
	/**
	 * @since 4.0
	 */
	def mergeToComplex[A](implicit ev1:Primitive <:< A, ev2:Complex <:< A):ParserRetVal[A,Nothing,ParserFailure,BuilderFailure] = {
		this.fold(
			{c => ParserRetVal.Complex(ev2(c))},
			{p => ParserRetVal.Complex(ev1(p))},
			{f => ParserRetVal.ParserFailure(f)},
			{b => ParserRetVal.BuilderFailure(b)}
		)
	}
	
	/**
	 * @since 4.0
	 */
	def flip:ParserRetVal[Primitive, Complex, ParserFailure, BuilderFailure] = this match {
		case ParserRetVal.Complex(x) => ParserRetVal.Primitive(x)
		case ParserRetVal.Primitive(x) => ParserRetVal.Complex(x)
		case x:ParserRetVal.ParserFailure[ParserFailure] => x
		case x:ParserRetVal.BuilderFailure[BuilderFailure] => x
	}
	
	/**
	 * Map Complex to a new type. Only avaliable if Primitive is Nothing
	 * @since 4.0
	 */
	def map[A](fun : Complex => A)(implicit ev:Primitive <:< Nothing):ParserRetVal[A,Primitive,ParserFailure,BuilderFailure] = this match {
		case ParserRetVal.Complex(x) => ParserRetVal.Complex(fun(x))
		case ParserRetVal.Primitive(x) => ParserRetVal.Primitive(x)
		case x:ParserRetVal.ParserFailure[ParserFailure] => x
		case x:ParserRetVal.BuilderFailure[BuilderFailure] => x
	}
	
	/**
	 * Map Primtive to a new type. Only avaliable if Complex is Nothing
	 * @since 4.0
	 */
	def map[A](fun : Primitive => A)(implicit ev:Complex <:< Nothing, ev2: Int =:= Int):ParserRetVal[Complex,A,ParserFailure,BuilderFailure] = this match {
		case ParserRetVal.Complex(x) => ParserRetVal.Complex(x)
		case ParserRetVal.Primitive(x) => ParserRetVal.Primitive(fun(x))
		case x:ParserRetVal.ParserFailure[ParserFailure] => x
		case x:ParserRetVal.BuilderFailure[BuilderFailure] => x
	}
	
	/**
	 * Flatmap Complex to a new type. Only avaliable if Primitive is Nothing
	 * @since 4.0
	 */
	def flatMap[XX, PP, FF >: ParserFailure, BB >: BuilderFailure](fun : Complex => ParserRetVal[XX, PP, FF, BB])(implicit ev:Primitive <:< Nothing):ParserRetVal[XX, PP, FF, BB] = this match {
		case ParserRetVal.Complex(x) => fun(x)
		case ParserRetVal.Primitive(x) => ParserRetVal.Primitive(ev(x))
		case x:ParserRetVal.ParserFailure[ParserFailure] => x
		case x:ParserRetVal.BuilderFailure[BuilderFailure] => x
	}
	
	/**
	 * Flatmap Primtive to a new type. Only avaliable if Complex is Nothing
	 * @since 4.0
	 */
	def flatMap[CC, XX, FF >: ParserFailure, BB >: BuilderFailure](fun : Primitive => ParserRetVal[CC, XX, FF, BB])(implicit ev:Complex <:< Nothing, ev2: Int =:= Int):ParserRetVal[CC, XX, FF, BB] = this match {
		case ParserRetVal.Complex(x) => ParserRetVal.Complex(ev(x))
		case ParserRetVal.Primitive(x) => fun(x)
		case x:ParserRetVal.ParserFailure[ParserFailure] => x
		case x:ParserRetVal.BuilderFailure[BuilderFailure] => x
	}
}

/**
 * A container for the types of [[ParserRetVal]]s
 * @since 3.0
 * @version 4.0
 */
object ParserRetVal {
	
	/** Represents a value that was produced by the parser without builder consultation */
	final case class Primitive[+P](x:P) extends ParserRetVal[Nothing, P, Nothing, Nothing]{
		def fold[Out](c:Function1[Nothing,Out], p:Function1[P,Out], f:Function1[Nothing,Out], b:Function1[Nothing,Out]):Out = p(x)
	}
	
	/** Represents a value that was produced via consultation of a builder */
	final case class Complex[+C](x:C) extends ParserRetVal[C, Nothing, Nothing, Nothing] {
		def fold[Out](c:Function1[C,Out], p:Function1[Nothing,Out], f:Function1[Nothing,Out], b:Function1[Nothing,Out]):Out = c(x)
	}
	
	/**
	 * Represents an attempt at parsing that was unsuccessful
	 * @since 4.0
	 */
	final case class ParserFailure[+F](x:F) extends ParserRetVal[Nothing, Nothing, F, Nothing] {
		def fold[Out](c:Function1[Nothing,Out], p:Function1[Nothing,Out], f:Function1[F,Out], b:Function1[Nothing,Out]):Out = f(x)
	}
	
	/**
	 * Represents an attempt at parsing that was unsuccessful
	 * @since 4.0
	 */
	final case class BuilderFailure[+B](x:B) extends ParserRetVal[Nothing, Nothing, Nothing, B] {
		def fold[Out](c:Function1[Nothing,Out], p:Function1[Nothing,Out], f:Function1[Nothing,Out], b:Function1[B,Out]):Out = b(x)
	}
	
	
	/** A projection as if the ParserRetVal were a primitive */
	final class PrimitiveProjection[+C,+P,+F,+B](backing:ParserRetVal[C,P,F,B]) {
		
		/** Map the backing value if the backing value is a Primitive, else return the backing value */
		def map[X](fun:P => X):ParserRetVal[C,X,F,B] = backing match {
			case Primitive(p) => Primitive(fun(p))
			case c:Complex[C] => c
			case f:ParserFailure[F] => f
			case b:BuilderFailure[B] => b
		}
		
		/** Flatmap the backing value if the backing value is a Primitive, else return the backing value */
		def flatMap[CC >: C, X, FF >: F, BB >: B](fun:P => ParserRetVal[CC, X, FF, BB]):ParserRetVal[CC, X, FF, BB] = backing match {
			case Primitive(p) => fun(p)
			case c:Complex[C] => c
			case f:ParserFailure[F] => f
			case b:BuilderFailure[B] => b
		}
	}
	
	/** A projection as if the ParserRetVal were a Complex */
	sealed class ComplexProjection[+C,+P,+F,+B](backing:ParserRetVal[C,P,F,B]) {
		
		/** Map the backing value if the backing value is a Complex, else return the backing value */
		def map[X](fun:C => X):ParserRetVal[X,P,F,B] = backing match {
			case Complex(c) => Complex(fun(c))
			case p:Primitive[P] => p
			case f:ParserFailure[F] => f
			case b:BuilderFailure[B] => b
		}
		
		/** Flatmap the backing value if the backing value is a Complex, else return the backing value
		 * @since 3.1
		 */
		def flatMap[PP >: P, X, FF >: F, BB >: B](fun:C => ParserRetVal[X, PP, FF, BB]):ParserRetVal[X, PP, FF, BB] = backing match {
			case Complex(c) => fun(c)
			case p:Primitive[P] => p
			case f:ParserFailure[F] => f
			case b:BuilderFailure[B] => b
		}
	}
	
	/** A projection as if the ParserRetVal were a Failure
	 * @since next
	 */
	sealed class ParserFailureProjection[+C,+P,+F,+B](backing:ParserRetVal[C,P,F,B]) {
		
		/** Map the backing value if the backing value is a Failure, else return the backing value */
		def map[X](fun:F => X):ParserRetVal[C,P,X,B] = backing match {
			case c:Complex[C] => c
			case p:Primitive[P] => p
			case ParserFailure(x) => ParserFailure(fun(x))
			case b:BuilderFailure[B] => b
		}
		
		/** Flatmap the backing value if the backing value is a ParserFailure, else return the backing value */
		def flatMap[CC >: C, PP >: P, X, BB >: B](fun:F => ParserRetVal[CC, PP, X, BB]):ParserRetVal[CC, PP, X, BB] = backing match {
			case c:Complex[C] => c
			case p:Primitive[P] => p
			case ParserFailure(f) => fun(f)
			case b:BuilderFailure[B] => b
		}
	}
	
	/** A projection as if the ParserRetVal were a Failure
	 * @since next
	 */
	sealed class BuilderFailureProjection[+C,+P,+F,+B](backing:ParserRetVal[C,P,F,B]) {
		
		/** Map the backing value if the backing value is a Failure, else return the backing value */
		def map[X](fun:B => X):ParserRetVal[C,P,F,X] = backing match {
			case c:Complex[C] => c
			case p:Primitive[P] => p
			case f:ParserFailure[F] => f
			case BuilderFailure(x) => BuilderFailure(fun(x))
		}
		
		/** Flatmap the backing value if the backing value is a BuilderFailure, else return the backing value */
		def flatMap[CC >: C, PP >: P, FF >: F, X](fun:B => ParserRetVal[CC, PP, FF, X]):ParserRetVal[CC, PP, FF, X] = backing match {
			case c:Complex[C] => c
			case p:Primitive[P] => p
			case f:ParserFailure[F] => f
			case BuilderFailure(x) => fun(x)
		}
	}
	
}
