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

import static java.nio.charset.StandardCharsets.UTF_8;
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
 * A JSONEncoder that can encode Map
 */
public final class MapJsonEncoder implements JSONEncoder
{
	public boolean canEncode(Object a) {
		if (a == null) {return false;}
		return (
			Map.class.isInstance(a)
		);
	}
	
	public String encode(Object a, JSONEncoder recurser) throws ClassCastException {
		if (a == null) {
			throw new ClassCastException("cannot encode null");
		} else if (a instanceof Map<?,?>) {
			Map<?,?> b = (Map<?,?>) a;
			
			if (b.size() == 0) {
				return "{}";
			} else {
				final StringBuilder sb = new StringBuilder("{");
				for (Map.Entry<?, ?> entry : b.entrySet()) {
					sb.append(
						JSONString.generateParsed(
								entry.getKey().toString()).getUnparsedString()
					);
					sb.append(':');
					sb.append( recurser.encode(entry.getValue(), recurser) );
					sb.append(',');
				}
				sb.setCharAt(sb.length() - 1, '}');
				return sb.toString();
			}
		} else {
			throw new ClassCastException("cannot encode object: " + a.toString());
		}
	}
	
}

