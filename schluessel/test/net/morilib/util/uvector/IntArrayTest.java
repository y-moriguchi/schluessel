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

public class IntArrayTest extends TC {
	
	static final int[] TEST0 = new int[] {
		0xcafebabe, 0x12345678,
	};
	static final int[] TEST1 = new int[] {
		0xaaaaaaaa, 0x12345678,
	};
	static final IntArray ARRS = IntArray.newArray(TEST0);
	static final IntArray ARRU = IntArray.newuArray(TEST0);
	
	public void testNewArray() {
		IntArray.newArray(TEST0);
		
		try {
			IntArray.newArray(null);  fail();
		} catch(NullPointerException e) {}
	}
	
	public void testNewuArray() {
		IntArray.newArray(TEST0);
		
		try {
			IntArray.newuArray(null);  fail();
		} catch(NullPointerException e) {}
	}
	
	public void testGetByte() {
		eq(ARRS.getByte(0), (byte)0xcafebabe);
		eq(ARRU.getByte(0), (byte)0xcafebabe);
	}
	
	public void testGetDouble() {
		eq(ARRS.getDouble(0), 0xcafebabe);
		eq(ARRU.getDouble(0), 0xcafebabel);
	}
	
	public void testGetFloat() {
		//eq(ARRS.getFloat(0), 0xcafebabe);
		//eq(ARRU.getFloat(0), 0xcafebabel);
	}
	
	public void testGetInt() {
		eq(ARRS.getInt(0), 0xcafebabe);
		eq(ARRU.getInt(0), 0xcafebabe);
		eq(ARRS.getInt(1), 0x12345678);
		eq(ARRU.getInt(1), 0x12345678);
		
		try {
			ARRS.getInt(2);  fail();
		} catch(IndexOutOfBoundsException e) {}
		
		try {
			ARRU.getInt(-1);  fail();
		} catch(IndexOutOfBoundsException e) {}
	}
	
	public void testGetLong() {
		eq(ARRS.getLong(0), 0xcafebabe);
		eq(ARRU.getLong(0), 0xcafebabel);
	}
	
	public void testGetShort() {
		eq(ARRS.getShort(0), (short)0xcafebabe);
		eq(ARRU.getShort(0), (short)0xcafebabe);
	}
	
	public void testSetByte() {
		IntArray s = new IntArray(ARRS);
		IntArray u = new IntArray(ARRU);
		int[] r1 = new int[] {
				(byte)0xaa, 0x12345678
		};
		int[] r2 = new int[] {
				0xaa, 0x12345678
		};
		
		s.setByte(0, (byte)0xaa);
		u.setByte(0, (byte)0xaa);
		ok(Arrays.equals(r1, s.toArray()));
		ok(Arrays.equals(r2, u.toArray()));
	}
	
	public void testSetDouble() {
		IntArray s = new IntArray(ARRS);
		IntArray u = new IntArray(ARRU);
		
		s.setDouble(0, 0xaaaaaaaa);
		u.setDouble(0, 0xaaaaaaaal);
		ok(Arrays.equals(TEST1, s.toArray()));
		ok(Arrays.equals(TEST1, u.toArray()));
	}
	
	public void testSetFloat() {
		IntArray s = new IntArray(ARRS);
		IntArray u = new IntArray(ARRU);
		
		s.setFloat(0, 0xaaaaaaaa);
		u.setFloat(0, 0xaaaaaaaal);
		//ok(Arrays.equals(TEST1, s.toArray()));
		//ok(Arrays.equals(TEST1, u.toArray()));
	}
	
	public void testSetInt() {
		IntArray s = new IntArray(ARRS);
		IntArray u = new IntArray(ARRU);
		
		s.setInt(0, 0xaaaaaaaa);
		u.setInt(0, 0xaaaaaaaa);
		ok(Arrays.equals(TEST1, s.toArray()));
		ok(Arrays.equals(TEST1, u.toArray()));
		
		try {
			s.setInt(2, 0);  fail();
		} catch(IndexOutOfBoundsException e) {}
		
		try {
			u.setInt(-1, 0);  fail();
		} catch(IndexOutOfBoundsException e) {}
	}
	
	public void testSetLong() {
		IntArray s = new IntArray(ARRS);
		IntArray u = new IntArray(ARRU);
		
		s.setLong(0, 0xaaaaaaaal);
		u.setLong(0, 0xaaaaaaaal);
		ok(Arrays.equals(TEST1, s.toArray()));
		ok(Arrays.equals(TEST1, u.toArray()));
	}
	
	public void testSetShort() {
		IntArray s = new IntArray(ARRS);
		IntArray u = new IntArray(ARRU);
		int[] r1 = new int[] {
				(short)0xaaaa, 0x12345678
		};
		int[] r2 = new int[] {
				0xaaaa, 0x12345678
		};
		
		s.setShort(0, (short)0xaaaa);
		u.setShort(0, (short)0xaaaa);
		ok(Arrays.equals(r1, s.toArray()));
		ok(Arrays.equals(r2, u.toArray()));
	}
	
	public void testToByteArray() {
		byte[] rb = new byte[] {
				(byte)0xca, (byte)0xfe, (byte)0xba, (byte)0xbe,
				(byte)0x12, (byte)0x34, (byte)0x56, (byte)0x78,
		};
		byte[] rl = new byte[] {
				(byte)0xbe, (byte)0xba, (byte)0xfe, (byte)0xca,
				(byte)0x78, (byte)0x56, (byte)0x34, (byte)0x12,
		};
		
		ok(Arrays.equals(rb, ARRS.toByteArray(Endianness.BIG)));
		ok(Arrays.equals(rl, ARRS.toByteArray(Endianness.LITTLE)));
	}
	
}
