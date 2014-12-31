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

import java.util.UUID;
import java.util.ArrayList;
import java.net.URI;
import com.rayrobdod.conciseBinaryObjectRepresentation.CBOREncoder;

/**
 * A JSONEncoder that can encode tagged elements
 */
public final class TaggedElemCborEncoder implements CBOREncoder
{
	private StringCborEncoder backing = new StringCborEncoder();
	
	public boolean canEncode(Object a) {
		if (a == null) {return false;}
		return (
			UUID.class.isInstance(a) ||
			URI.class.isInstance(a) ||
			false
		);
	}
	
	public byte[] encode(Object a, CBOREncoder recurser) throws ClassCastException {
		final byte majorType = (byte) (6 << 5);
		
		if (a == null) {
			throw new ClassCastException("cannot encode null");
			
		} else if (a instanceof URI) {
			final byte firstByte = (byte) (majorType | 24);
			final byte secondByte = 32;
			
			URI value = (URI) a;
			
			return merge(firstByte, secondByte, backing.encode(value.toString(), recurser));
			
		} else if (a instanceof UUID) {
			final byte firstByte = (byte) (majorType | 24);
			final byte secondByte = 37;
			
			UUID value = (UUID) a;
			byte[] valueBytes = merge(
					separateBytes(value.getMostSignificantBits()),
					separateBytes(value.getLeastSignificantBits())
			);
			
			
			return merge(firstByte, secondByte, backing.encode(valueBytes, recurser));
			
		} else {
			throw new ClassCastException("cannot encode object: " + a.toString());
		}
	}
	
	
	private static byte[] separateBytes(Long l) {
		final ArrayList<Byte> list = new ArrayList<Byte>(8);
		for (int i = 56; i >= 0; i -= 8) {
			list.add( (byte) ((l >> i) & (0xFF)) );
		}
		Byte[] stageOne = list.toArray(new Byte[0]);
		byte[] stageTwo = new byte[stageOne.length];
		
		for (int i = 0; i < stageOne.length; i++) {
			stageTwo[i] = stageOne[i];
		}
		return stageTwo;
	}
	
	private static byte[] merge(byte a, byte b, byte[] c) {
		byte[] out = new byte[2 + c.length];
		out[0] = a;
		out[1] = b;
		for (int i = 0; i < c.length; i++) {
			out[2 + i] = c[i];
		}
		return out;
	}
	
	private static byte[] merge(byte[] a, byte[] b) {
		byte[] out = new byte[a.length + b.length];
		for (int i = 0; i < a.length; i++) {
			out[i] = a[i];
		}
		for (int i = 0; i < b.length; i++) {
			out[a.length + i] = b[i];
		}
		return out;
	}
}


