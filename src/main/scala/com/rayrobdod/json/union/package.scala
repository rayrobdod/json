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
package com.rayrobdod.json

/**
 * Contains various tagged unions that are used in preference to Any-promotion.
 */
package object union {
}

package union {
	sealed trait PiecewiseBuilderFailures
	
	package Failures {
		/** When a Builder wanted a value of one type but was given a value of a different type */
		final case class UnsuccessfulTypeCoersion(value:Any, fromType:String, toType:String) extends PiecewiseBuilderFailures
		/** When a Builder wanted a Primtive value, but was given a Complex one */
		object ExpectedPrimitive extends PiecewiseBuilderFailures
		/** When a Builder wanted a Complex value, but was given a Primitive one */
		object ExpectedComplex extends PiecewiseBuilderFailures
		/** Raised when Builder::apply's folding parameter is unusable; when it detectably violates the contract of (returned from Builder::init or Builder::apply) */
		object IllegalFoldingInBuilder extends PiecewiseBuilderFailures
		object UnknownKey extends PiecewiseBuilderFailures
		/** Rised by parsers or builders that always throw exceptions - i.e. test cases */
		private[json] object EnforcedFailure
		/** A Failure with additional location information */
		final case class Indexed[A](cause:A, charIndex:Int)
	}
	
}
