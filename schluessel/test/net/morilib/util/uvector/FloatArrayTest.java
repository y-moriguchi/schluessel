/*
 * Copyright 2009 Yuichiro Moriguchi
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package net.morilib.util.uvector;

import java.util.Arrays;

import net.morilib.lisp.test.TC;

public class FloatArrayTest extends TC {
	
	static final float[] TEST0 = new float[] {
		12.25f
	};
	static final float[] TEST1 = new float[] {
		Float.intBitsToFloat(0xcafebabe)
	};
	static final FloatArray ARRS = FloatArray.newArray(TEST0);
	static final FloatArray ARR2 = FloatArray.newArray(TEST1);
	
	public void testNewArray() {
		FloatArray.newArray(TEST0);
		
		try {
			FloatArray.newArray(null);  fail();
		} catch(NullPointerException e) {}
	}
	
	public void testGetByte() {
		eq(ARRS.getByte(0), 12);
	}
	
	public void testGetDouble() {
		eq(ARRS.getDouble(0), 12.25);
	}
	
	public void testGetFloat() {
		eq(ARRS.getFloat(0), 12.25);
	}
	
	public void testGetInt() {
		eq(ARRS.getInt(0), 12);
		
		try {
			ARRS.getInt(1);  fail();
		} catch(IndexOutOfBoundsException e) {}
	}
	
	public void testGetLong() {
		eq(ARRS.getLong(0), 12);
	}
	
	public void testGetShort() {
		eq(ARRS.getShort(0), 12);
	}
	
	public void testSetByte() {
		FloatArray s = new FloatArray(ARRS);
		float[] r1 = new float[] {
				(byte)0xaa
		};
		
		s.setByte(0, (byte)0xaa);
		ok(Arrays.equals(r1, s.toArray()));
	}
	
	public void testSetDouble() {
		FloatArray s = new FloatArray(ARRS);
		float[] r1 = new float[] {
				23.45f
		};
		
		s.setDouble(0, 23.45);
		eq(r1, s.toArray());
	}
	
	public void testSetFloat() {
		FloatArray s = new FloatArray(ARRS);
		float[] r1 = new float[] {
				23.45f
		};
		
		s.setFloat(0, 23.45f);
		eq(r1, s.toArray());
		
		try {
			s.setFloat(1, 0);  fail();
		} catch(IndexOutOfBoundsException e) {}
		
		try {
			s.setFloat(-1, 0);  fail();
		} catch(IndexOutOfBoundsException e) {}
	}
	
	public void testSetInt() {
		FloatArray s = new FloatArray(ARRS);
		float[] r1 = new float[] {
				(int)0xaaaaaaaa
		};
				
		s.setInt(0, 0xaaaaaaaa);
		eq(r1, s.toArray());
	}
	
	public void testSetLong() {
		FloatArray s = new FloatArray(ARRS);
		float[] r1 = new float[] {
				0xaaaaaaaaaaaaaaaal
		};
				
		s.setLong(0, 0xaaaaaaaaaaaaaaaal);
		ok(Arrays.equals(r1, s.toArray()));
	}
	
	public void testSetShort() {
		FloatArray s = new FloatArray(ARRS);
		float[] r1 = new float[] {
				(short)0xaaaa
		};
		
		s.setShort(0, (short)0xaaaa);
		eq(r1, s.toArray());
	}
	
	public void testToByteArray() {
		byte[] rb = new byte[] {
				(byte)0xca, (byte)0xfe, (byte)0xba, (byte)0xbe,
		};
		byte[] rl = new byte[] {
				(byte)0xbe, (byte)0xba, (byte)0xfe, (byte)0xca
		};
		
		eq(rb, ARR2.toByteArray(Endianness.BIG));
		eq(rl, ARR2.toByteArray(Endianness.LITTLE));
	}
	
}
