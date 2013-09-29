/*
	Copyright (c) 2013, Raymond Dodge
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

package com.rayrobdod.binaryJSON;

import static java.nio.charset.StandardCharsets.UTF_8;
import java.util.Map;
import java.util.List;
import java.io.DataOutput;
import java.io.IOException;
import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.text.ParseException;

/**
 * Writes a data structure to a file in BSON format
 * 
 * @author Raymond Dodge
 * @version 2013 Aug 03
 * @version 2013 Aug 05 - forgot a break
 * @see BSONParser
 */
public final class BSONWriter
{
	private BSONWriter() {}
	
	public static byte[] encode(Map<String, ?> data)
			throws NullPointerException, IOException, NulCharacterInKeyException
	{
		ByteArrayOutputStream os = new ByteArrayOutputStream();
		DataOutputStream dos = new DataOutputStream(os);
		
		for (Map.Entry<String, ?> entry : data.entrySet()) {
			byte type = objectType(entry.getValue());
			
			byte[] key = entry.getKey().getBytes(UTF_8);
			
			if (entry.getKey().indexOf('\0') != -1) {
				throw new NulCharacterInKeyException(entry.getKey());
			}
			
			dos.writeByte(type);
			dos.write(key);
			dos.writeByte(0);
			
			writeValue(type, dos, entry.getValue());
		}
		dos.writeByte(0);
		dos.flush();
		dos.close();
		byte[] osba = os.toByteArray();
		
		ByteArrayOutputStream os2 = new ByteArrayOutputStream();
		DataOutputStream dos2 = new DataOutputStream(os2);
		
		dos2.writeInt(Integer.reverseBytes( osba.length + 4) );
		dos2.write(osba);
		dos2.flush();
		dos2.close();
		
		return os2.toByteArray();
	}
	
	public static byte[] encode(List<?> data)
			throws NullPointerException, IOException
	{
		ByteArrayOutputStream os = new ByteArrayOutputStream();
		DataOutputStream dos = new DataOutputStream(os);
		int keyInt = 0;
		
		for (Object value : data) {
			byte type = objectType(value);
			byte[] key = Integer.toString(keyInt).getBytes(UTF_8);
			
			dos.writeByte(type);
			dos.write(key);
			dos.writeByte(0);
			
			writeValue(type, dos, value);
			keyInt++;
		}
		dos.writeByte(0);
		dos.flush();
		dos.close();
		byte[] osba = os.toByteArray();
		
		ByteArrayOutputStream os2 = new ByteArrayOutputStream();
		DataOutputStream dos2 = new DataOutputStream(os2);
		
		dos2.writeInt(Integer.reverseBytes( osba.length + 4) );
		dos2.write(osba);
		dos2.flush();
		dos2.close();
		
		return os2.toByteArray();
	}
	
	private static byte objectType(Object o) {
		if (o == null)            { return 0x0A; }
		if (o instanceof Byte)    { return 0x10; }
		if (o instanceof Short)   { return 0x10; }
		if (o instanceof Integer) { return 0x10; }
		if (o instanceof Long)    { return 0x12; }
		if (o instanceof Float)   { return 0x01; }
		if (o instanceof Double)  { return 0x01; }
		if (o instanceof Boolean) { return 0x08; }
		if (o instanceof byte[])  { return 0x05; }
		if (o instanceof CharSequence) { return 0x02; }
		if (o instanceof Map)     { return 0x03; }
		if (o instanceof List)    { return 0x04; }
		
		throw new UnsupportedOperationException(o.getClass().getName());
	}
	
	public static void writeValue(byte type, DataOutput output, Object o)
			throws IOException {
		Integer i;
		
		switch (type) {
			case 0x0A : break;
				
			case 0x01 : { // double
				double d = ((Number) o).doubleValue();
				long l = Double.doubleToLongBits(d);
				l = Long.reverseBytes(l);
				output.writeLong(l);
				break;
			}
			case 0x12 : { // long
				long l = ((Number) o).longValue();
				l = Long.reverseBytes(l);
				output.writeLong(l);
				break;
			}
			case 0x09 : { // timestamp
				long l = ((java.util.Date) o).getTime();
				l = Long.reverseBytes(l);
				output.writeLong(l);
				break;
			}
			case 0x10 : { // integer
				int l = ((Number) o).intValue();
				l = Integer.reverseBytes(l);
				output.writeInt(l);
				break;
			}
			case 0x08 : { // boolean
				boolean b = ((Boolean) o).booleanValue();
				output.writeByte((b?0x1:0x0));
				break;
			}
			case 0x05 : { // byte[]
				byte[] b = ((byte[]) o);
				output.write(b);
				break;
			}
			case 0x02 : { // string
				String s = o.toString();
				byte[] data = s.getBytes(UTF_8);
				
				output.writeInt(Integer.reverseBytes( data.length + 1 ));
				output.write(data);
				output.writeByte(0);
				break;
			}
			case 0x03 : { // object
				Map<?,?> m1 = ((Map<?,?>) o);
				Map<String, Object> m2 = new java.util.HashMap<String,Object>();
				
				for (Map.Entry<?,?> e : m1.entrySet()) {
					m2.put( e.getKey().toString(), e.getValue() );
				}
				output.write( encode(m2) );
				break;
			}
			case 0x04 : { // array
				List<?> m1 = ((List<?>) o);
				output.write( encode(m1) );
				break;
			}
		}
	}
}
