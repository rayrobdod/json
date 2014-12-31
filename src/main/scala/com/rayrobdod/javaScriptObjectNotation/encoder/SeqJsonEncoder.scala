/*
	Copyright (c) 2014, Raymond Dodge
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
package com.rayrobdod.javaScriptObjectNotation.encoder;

import java.nio.charset.StandardCharsets.UTF_8;
import java.util.Map;
import java.util.List;
import java.io.DataOutput;
import java.io.IOException;
import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.text.ParseException;
import com.rayrobdod.javaScriptObjectNotation.JSONEncoder;
import com.rayrobdod.javaScriptObjectNotation.JSONString;

/**
 * A JSONEncoder that can encode Seqs
 */
final class SeqJsonEncoder extends JSONEncoder
{
	def canEncode(a:Object):Boolean = a match {
		case null => false
		case x:Seq[_] => true
		case _ => false
	}
	
	def encode(a:Object, rec:JSONEncoder):String = a match {
		case null => throw new ClassCastException("cannot encode null");
		case b:Seq[_] => '[' +: b.map{x:Any => rec.encode(x, rec)}.foldLeft(""){_ + "," + _}.drop(1) :+ ']'
		case _ => throw new ClassCastException("cannot encode object: " + a.toString())
	}
}

