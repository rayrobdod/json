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

import java.util.ArrayList;
import com.rayrobdod.conciseBinaryObjectRepresentation.CBOREncoder;

/**
 * A JSONEncoder that can encode numbers, booleans and 'null'.
 */
public final class PrimitiveCborEncoder implements CBOREncoder
{
	public boolean canEncode(Object a) {
		if (a == null) {return true;}
		return (
			Number.class.isInstance(a)  ||
			Boolean.class.isInstance(a) ||
			false
		);
	}
	
	public byte[] encode(Object a, CBOREncoder recurser) throws ClassCastException {
		if (a == null) {
			byte[] retVal = { (byte) (0xf6) };
			return retVal;
			
		} else if (a instanceof Boolean) {
			boolean b = (Boolean) a;
			byte[] retVal = { (byte) (b ? 0xf5 : 0xf4) };
			return retVal;
			
		} else if (a instanceof Number) {
			Number b = (Number) a;
			long l   = b.longValue();
			double d = b.doubleValue();
			float f  = b.floatValue();
			
			if (d != l && d != f) {
				// does not fit in long or float
				final byte firstByte = (byte) (0xfb);
				
				return PrimitiveCborEncoder.merge(firstByte, separateBytes(
						Double.doubleToLongBits(d)));
				
			} else if (f != l) {
				// does not fit in integral type
				final byte firstByte = (byte) (0xfa);
				
				return PrimitiveCborEncoder.merge(firstByte, separateBytes(
						Float.floatToIntBits(f)));
				
			} else {
				// integral number
				int majorType = ( l >= 0 ? 0 : 1 ) << 5;
				long abs = ( l >= 0 ? l : (-1 - l) );
				
				if (abs <= 23) {
					byte[] retVal = {(byte) (majorType | abs)};
					return retVal;
				} else if (abs <= 255) {
					byte[] retVal = {(byte) (majorType | 24), (byte) abs};
					return retVal;
				} else if (abs <= 0xFFFF) {
					byte[] retVal = {(byte) (majorType | 25), (byte) ((abs >> 8) & 0xFF), (byte) (abs & 0xFF)};
					return retVal;
				} else if (abs <= 0xFFFFFFFF) {
					return PrimitiveCborEncoder.merge((byte) (majorType | 26), separateBytes((int) abs));
				} else {
					return PrimitiveCborEncoder.merge((byte) (majorType | 27), separateBytes(abs));
				}
			}
			
		} else {
			throw new ClassCastException("cannot encode object: " + a.toString());
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
	
	private static byte[] merge(byte a, Byte[] b) {
		byte[] out = new byte[1 + b.length];
		out[0] = a;
		for (int i = 0; i < b.length; i++) {
			out[1 + i] = b[i];
		}
		return out;
	}
	
}


