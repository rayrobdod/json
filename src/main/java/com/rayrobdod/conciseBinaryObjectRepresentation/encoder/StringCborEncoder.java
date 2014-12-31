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
package com.rayrobdod.conciseBinaryObjectRepresentation.encoder;

import static java.nio.charset.StandardCharsets.UTF_8;
import java.util.ArrayList;
import com.rayrobdod.conciseBinaryObjectRepresentation.CBOREncoder;

/**
 * A JSONEncoder that can encode byte and text strings
 */
public final class StringCborEncoder implements CBOREncoder
{
	public boolean canEncode(Object a) {
		if (a == null) {return false;}
		return (
			byte[].class.isInstance(a)  ||
			CharSequence.class.isInstance(a) ||
			false
		);
	}
	
	public byte[] encode(Object a, CBOREncoder recurser) throws ClassCastException {
		byte majorType;
		byte[] values;
		
		if (a == null) {
			throw new ClassCastException("cannot encode null");
			
		} else if (a instanceof byte[]) {
			majorType = 2 << 5;
			values = (byte[]) a;
			
		} else if (a instanceof CharSequence) {
			majorType = 3 << 5;
			values = a.toString().getBytes(UTF_8);
			
		} else {
			throw new ClassCastException("cannot encode object: " + a.toString());
		}
		
		if (values.length <= 23) {
			return merge( (byte) (majorType | values.length), new Byte[0], values );
		} else if (values.length <= 255) {
			return merge( (byte) (majorType | 24), new Byte[] {(byte) values.length}, values );
		} else if (values.length <= 0xFFFF) {
			return merge( (byte) (majorType | 25), new Byte[] {(byte) (values.length >> 8), (byte) values.length}, values );
		} else {
			return merge( (byte) (majorType | 26), separateBytes(values.length), values );
		}
		
	}
	
	
	// (56 to 0 by -8).map{i => ((x >> i) & (0xFF)).toHexString}
	// (0 to 64 by 8).map{i => ((x >> i) & (0xFF)).toHexString}
	private static Byte[] separateBytes(Long l) {
		final ArrayList<Byte> list = new ArrayList<Byte>(8);
		for (int i = 56; i >= 0; i -= 8) {
			list.add( (byte) ((l >> i) & (0xFF)) );
		}
		return list.toArray(new Byte[0]);
	}
	
	private static Byte[] separateBytes(Integer l) {
		final ArrayList<Byte> list = new ArrayList<Byte>(4);
		for (int i = 24; i >= 0; i -= 8) {
			list.add( (byte) ((l >> i) & (0xFF)) );
		}
		return list.toArray(new Byte[0]);
	}
	
	private static byte[] merge(byte a, Byte[] b, byte[] c) {
		byte[] out = new byte[1 + b.length + c.length];
		out[0] = a;
		for (int i = 0; i < b.length; i++) {
			out[1 + i] = b[i];
		}
		for (int i = 0; i < c.length; i++) {
			out[1 + b.length + i] = c[i];
		}
		return out;
	}
	
}


