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

import java.util.List;
import java.util.ArrayList;
import com.rayrobdod.conciseBinaryObjectRepresentation.CBOREncoder;

/**
 * A CBOREncoder that acts as the union of the provided CBOREncoders
 */
public final class AggregateCborEncoder implements CBOREncoder
{
	private final List<CBOREncoder> childs;
	
	public AggregateCborEncoder(List<CBOREncoder> childs) {
		this.childs = java.util.Collections.unmodifiableList(new java.util.ArrayList<CBOREncoder>(childs));
	}
	
	public boolean canEncode(Object a) {
		boolean retVal = false;
		for (CBOREncoder c : childs ) {
			retVal = retVal || c.canEncode(a);
		}
		return retVal;
	}
	
	public byte[] encode(Object a, CBOREncoder recurser) throws ClassCastException {
		byte[] retVal = null;
		for (CBOREncoder c : childs ) {
			if (c.canEncode(a)) {
				retVal = c.encode(a, recurser);
				break;
			}
		}
		
		if (retVal == null) {
			throw new ClassCastException("cannot encode object: " + a.toString());
		} else {
			return retVal;
		}
	}
	
}


