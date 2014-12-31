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
package com.rayrobdod.binaryJSON.encoder;

import static java.nio.charset.StandardCharsets.UTF_8;
import java.util.List;
import java.util.ArrayList;
import com.rayrobdod.binaryJSON.BSONEncoder;

/**
 * A JSONEncoder that can encode numbers, booleans and 'null'.
 */
public final class PrimitiveBsonEncoder implements BSONEncoder
{
	public boolean canEncode(Object a) {
		if (a == null) {return true;}
		return (
			Number.class.isInstance(a)  ||
			Boolean.class.isInstance(a) ||
			byte[].class.isInstance(a)  ||
			false
		);
	}
	
	public byte[] encode(Object a, byte[] key, BSONEncoder recurser) throws ClassCastException {
		if (a == null) {
			return merge((byte) 0x0A, key, new Byte[0]);
			
		} else if (a instanceof Number) {
			Number b = (Number) a;
			int i  = b.intValue();
			long l = b.longValue();
			double d = b.doubleValue();
			
			if (d != l) {
				// does not fit in long
				
				long bits = Double.doubleToLongBits(d);
				return PrimitiveBsonEncoder.merge((byte) 0x01,
					key,
					separateBytes(bits)
				);
			} else if (l != i) {
				// does not fit in int
				return PrimitiveBsonEncoder.merge((byte) 0x12,
					key,
					separateBytes(l)
				);
			} else {
				// does fit in int
				return PrimitiveBsonEncoder.merge((byte) 0x10,
					key,
					truncate(separateBytes(l), 4)
				);
			}
			
		} else if (a instanceof Boolean) {
			boolean b = (Boolean) a;
			Byte[] c = {(Byte)(byte) (b ? 1 : 0)};
			return PrimitiveBsonEncoder.merge((byte) 0x08, key, c);
			
		} else {
			throw new ClassCastException("cannot encode object: " + a.toString());
		}
	}
	
	
	// (56 to 0 by -8).map{i => ((x >> i) & (0xFF)).toHexString}
	// (0 to 64 by 8).map{i => ((x >> i) & (0xFF)).toHexString}
	private static Byte[] separateBytes(Long l) {
		final ArrayList<Byte> list = new ArrayList<Byte>(8);
		for (int i = 0; i < 64; i += 8) {
			list.add( (byte) ((l >> i) & (0xFF)) );
		}
		return list.toArray(new Byte[0]);
	}
	
	/** @pre len < in.length */
	private static Byte[] truncate(Byte[] in, int len) {
		Byte[] out = new Byte[len];
		for (int i = 0; i < len; i++) {
			out[i] = in[i];
		}
		return out;
	}
	
	private static byte[] merge(byte a, byte[] b, Byte[] c) {
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

