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

public class ShortArrayTest extends TC {
	
	static final short[] TEST0 = new short[] {
		(short)0xcafe, (short)0xbabe, (short)0x1234, (short)0x5678,
	};
	static final short[] TEST1 = new short[] {
		(short)0xaaaa, (short)0xbabe, (short)0x1234, (short)0x5678,
	};
	static final ShortArray ARRS = ShortArray.newArray(TEST0);
	static final ShortArray ARRU = ShortArray.newuArray(TEST0);
	
	public void testNewArray() {
		ShortArray.newArray(TEST0);
		
		try {
			ShortArray.newArray(null);  fail();
		} catch(NullPointerException e) {}
	}
	
	public void testNewuArray() {
		ShortArray.newArray(TEST0);
		
		try {
			ShortArray.newuArray(null);  fail();
		} catch(NullPointerException e) {}
	}
	
	public void testGetByte() {
		eq(ARRS.getByte(0), (byte)0xcafe);
		eq(ARRU.getByte(0), (byte)0xcafe);
	}
	
	public void testGetDouble() {
		eq(ARRS.getDouble(0), (short)0xcafe);
		eq(ARRU.getDouble(0), 0xcafe);
	}
	
	public void testGetFloat() {
		eq(ARRS.getFloat(0), (short)0xcafe);
		eq(ARRU.getFloat(0), 0xcafe);
	}
	
	public void testGetInt() {
		eq(ARRS.getInt(0), (short)0xcafe);
		eq(ARRU.getInt(0), 0xcafe);
	}
	
	public void testGetLong() {
		eq(ARRS.getLong(0), (short)0xcafe);
		eq(ARRU.getLong(0), 0xcafe);
	}
	
	public void testGetShort() {
		eq(ARRS.getShort(0), (short)0xcafe);
		eq(ARRU.getShort(0), (short)0xcafe);
		eq(ARRS.getShort(3), (short)0x5678);
		eq(ARRU.getShort(3), (short)0x5678);
		
		try {
			ARRS.getShort(4);  fail();
		} catch(IndexOutOfBoundsException e) {}
		
		try {
			ARRU.getShort(-1);  fail();
		} catch(IndexOutOfBoundsException e) {}
	}
	
	public void testSetByte() {
		ShortArray s = new ShortArray(ARRS);
		ShortArray u = new ShortArray(ARRU);
		short[] r1 = new short[] {
				(short)(byte)0xaa, (short)0xbabe,
				(short)0x1234, (short)0x5678,
		};
		short[] r2 = new short[] {
				(short)0xaa, (short)0xbabe,
				(short)0x1234, (short)0x5678,
		};
		
		s.setByte(0, (byte)0xaa);
		u.setByte(0, (byte)0xaa);
		ok(Arrays.equals(r1, s.toArray()));
		ok(Arrays.equals(r2, u.toArray()));
	}
	
	public void testSetDouble() {
		ShortArray s = new ShortArray(ARRS);
		ShortArray u = new ShortArray(ARRU);
		
		s.setDouble(0, (short)0xaaaa);
		u.setDouble(0, 0xaaaa);
		ok(Arrays.equals(TEST1, s.toArray()));
		ok(Arrays.equals(TEST1, u.toArray()));
	}
	
	public void testSetFloat() {
		ShortArray s = new ShortArray(ARRS);
		ShortArray u = new ShortArray(ARRU);
		
		s.setFloat(0, (short)0xaaaa);
		u.setFloat(0, 0xaaaa);
		ok(Arrays.equals(TEST1, s.toArray()));
		ok(Arrays.equals(TEST1, u.toArray()));
	}
	
	public void testSetInt() {
		ShortArray s = new ShortArray(ARRS);
		ShortArray u = new ShortArray(ARRU);
		
		s.setInt(0, (short)0xaaaa);
		u.setInt(0, 0xaaaa);
		ok(Arrays.equals(TEST1, s.toArray()));
		ok(Arrays.equals(TEST1, u.toArray()));
	}
	
	public void testSetLong() {
		ShortArray s = new ShortArray(ARRS);
		ShortArray u = new ShortArray(ARRU);
		
		s.setLong(0, (short)0xaaaa);
		u.setLong(0, 0xaaaa);
		ok(Arrays.equals(TEST1, s.toArray()));
		ok(Arrays.equals(TEST1, u.toArray()));
	}
	
	public void testSetShort() {
		ShortArray s = new ShortArray(ARRS);
		ShortArray u = new ShortArray(ARRU);
		
		s.setShort(0, (short)0xaaaa);
		u.setShort(0, (short)0xaaaa);
		ok(Arrays.equals(TEST1, s.toArray()));
		ok(Arrays.equals(TEST1, u.toArray()));
		
		try {
			s.getByte(4);  fail();
		} catch(IndexOutOfBoundsException e) {}
		
		try {
			u.getByte(-1);  fail();
		} catch(IndexOutOfBoundsException e) {}
	}
	
	public void testToByteArray() {
		byte[] rb = new byte[] {
				(byte)0xca, (byte)0xfe, (byte)0xba, (byte)0xbe,
				(byte)0x12, (byte)0x34, (byte)0x56, (byte)0x78,
		};
		byte[] rl = new byte[] {
				(byte)0xfe, (byte)0xca, (byte)0xbe, (byte)0xba,
				(byte)0x34, (byte)0x12, (byte)0x78, (byte)0x56,
		};
		
		ok(Arrays.equals(rb, ARRS.toByteArray(Endianness.BIG)));
		ok(Arrays.equals(rl, ARRS.toByteArray(Endianness.LITTLE)));
	}
	
}
