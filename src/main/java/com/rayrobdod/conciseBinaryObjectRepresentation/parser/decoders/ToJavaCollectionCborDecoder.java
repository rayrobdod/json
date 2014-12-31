/*
	Copyright (c) 2012-2013, Raymond Dodge
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
package com.rayrobdod.conciseBinaryObjectRepresentation.parser.decoders;

import static java.nio.charset.StandardCharsets.UTF_8;
import com.rayrobdod.conciseBinaryObjectRepresentation.parser.CBORDecoder;
import java.text.ParseException;

/**
 * This takes a String that is properly JSONEncoded and turns it into a Java Collections object
 * @author Raymond Dodge
 */
public final class ToJavaCollectionCborDecoder implements CBORDecoder
{
	/**
	 * This returns a decoding of a valid byte sequence into a Java Object
	 * @param c the CharSequence to decode
	 * @return a HashMap, ArrayList, JSONString, Number, Boolean or null
	 * @throws NullPointerException if c is null
	 * @throws ClassCastException if the string is does not match any associate types
	 */
	public Object decode(byte[] c) throws NullPointerException, ClassCastException
	{
		if (c == null) throw new NullPointerException();
		
		int majorType = (c[0] & 0xE0) >> 5;
		int additionalInfo = c[0] & 0x1F;
		long length = 0;
		int lengthlen;
		if (additionalInfo <= 23) {
			length = additionalInfo;
			lengthlen = 0;
		} else if (additionalInfo == 24) {
			lengthlen = 1;
		} else if (additionalInfo == 25) {
			lengthlen = 2;
		} else if (additionalInfo == 26) {
			lengthlen = 4;
		} else if (additionalInfo == 27) {
			lengthlen = 8;
		} else if (additionalInfo == 31) {
			// unknown length
			length = -1;
			lengthlen = 0;
		} else {
			throw new IllegalArgumentException("Disallowed number length");
		}
		
		for (int i = 0; i < lengthlen; i++) {
			long addend = ((long) c[1+i]) & 0xFF;
			length = (length << 8) + addend;
		}
		
		
		if (majorType == 0 || majorType == 1) {
			if (additionalInfo == 31) throw new IllegalArgumentException("Numbers not allowed to be unknown length");
			
			int signAddend = -majorType;
			int signMultiplier = signAddend * 2 + 1;
			return signAddend + signMultiplier * length;
		} else if (majorType == 2) {
			if (additionalInfo == 31) throw new IllegalArgumentException("Cant handle unknown length yet");
			
			byte[] a = new byte[(int) length];
			System.arraycopy(c, lengthlen + 1, a, 0, (int) length);
			return a;
		} else if (majorType == 3) {
			if (additionalInfo == 31) throw new IllegalArgumentException("Cant handle unknown length yet");
			
			return new String(c, lengthlen + 1, (int) length, UTF_8);
		} else if (majorType == 7) {
			if (additionalInfo == 20) { return false; }
			if (additionalInfo == 21) { return true; }
			if (additionalInfo == 22) { return null; }
			if (additionalInfo == 25) { throw new IllegalArgumentException("Can't handle half-precision"); }
			if (additionalInfo == 26) { return Float.intBitsToFloat((int) length); }
			if (additionalInfo == 27) { return Double.longBitsToDouble(length); }
			
			throw new IllegalArgumentException("Unknown Simple Value");
		} else {
			throw new IllegalArgumentException("Unknown Item Type");
		}
		
	}
}
