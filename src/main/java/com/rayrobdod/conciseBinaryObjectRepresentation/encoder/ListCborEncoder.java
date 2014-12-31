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
import java.util.List;
import java.util.ArrayList;
import com.rayrobdod.conciseBinaryObjectRepresentation.CBOREncoder;

/**
 * A JSONEncoder that can encode lists
 */
public final class ListCborEncoder implements CBOREncoder
{
	public boolean canEncode(Object a) {
		if (a == null) {return false;}
		return List.class.isInstance(a);
	}
	
	public byte[] encode(Object a, CBOREncoder recurser) throws ClassCastException {
		final byte majorType = (byte) (4 << 5);
		
		if (a == null) {
			throw new ClassCastException("cannot encode null");
			
		} else if (a instanceof List<?>) {
			List<?> values = (List<?>) a;
			
			ArrayList<byte[]> buffer = new ArrayList<byte[]>();
			
			if (values.size() <= 23) {
				final byte[] firstByte = { (byte) (majorType | values.size()) };
				buffer.add(firstByte);
			} else if (values.size() <= 255) {
				final byte[] firstBytes = { (byte) (majorType | 24), (byte) values.size() };
				buffer.add(firstBytes);
			} else if (values.size() <= 0xFFFF) {
				final byte[] firstBytes = { (byte) (majorType | 25), (byte) (values.size() >> 8), (byte) values.size() };
				buffer.add(firstBytes);
			} else {
				final byte[] firstByte = { (byte) (majorType | 26) };
				buffer.add(firstByte);
				buffer.add(separateBytes(values.size()));
			}
			
			for (Object o : values) { buffer.add(recurser.encode(o, recurser)); }
			
			int retValLen = 0;
			for (byte[] o : buffer) { retValLen += o.length; }
			byte[] retVal = new byte[retValLen];
			
			int position = 0;
			for (byte[] o : buffer) {
				for (int i = 0; i < o.length; i++) {
					retVal[position + i] = o[i];
				}
				position += o.length;
			}
			return retVal;
			
		} else {
			throw new ClassCastException("cannot encode object: " + a.toString());
		}
	}
	
	
	
	private static byte[] separateBytes(int l) {
		final ArrayList<Byte> list = new ArrayList<Byte>(4);
		for (int i = 24; i >= 0; i -= 8) {
			list.add( (byte) ((l >> i) & (0xFF)) );
		}
		Byte[] stageOne = list.toArray(new Byte[0]);
		byte[] stageTwo = new byte[stageOne.length];
		
		for (int i = 0; i < stageOne.length; i++) {
			stageTwo[i] = stageOne[i];
		}
		return stageTwo;
	}
	
}


