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
import net.morilib.util.Bytes;

public class LongArrayTest extends TC {
	
	static final long[] TEST0 = new long[] {
		0xcafebabe12345678l
	};
	static final long[] TEST1 = new long[] {
		0xaaaaaaaaaaaaaaaal
	};
	static final LongArray ARRS = LongArray.newArray(TEST0);
	static final LongArray ARRU = LongArray.newuArray(TEST0);
	
	public void testNewArray() {
		LongArray.newArray(TEST0);
		
		try {
			LongArray.newArray(null);  fail();
		} catch(NullPointerException e) {}
	}
	
	public void testNewuArray() {
		LongArray.newArray(TEST0);
		
		try {
			LongArray.newuArray(null);  fail();
		} catch(NullPointerException e) {}
	}
	
	public void testGetByte() {
		eq(ARRS.getByte(0), (byte)0xcafebabe12345678l);
		eq(ARRU.getByte(0), (byte)0xcafebabe12345678l);
	}
	
	public void testGetDouble() {
		eq(ARRS.getDouble(0), 0xcafebabe12345678l);
		eq(ARRU.getDouble(0),
				Bytes.ulongToDouble(0xcafebabe12345678l));
	}
	
	public void testGetFloat() {
		//eq(ARRS.getFloat(0), 0xcafebabe12345678l);
		eq(ARRU.getFloat(0),
				Bytes.ulongToFloat(0xcafebabe12345678l));
	}
	
	public void testGetInt() {
		eq(ARRS.getInt(0), (int)0xcafebabe12345678l);
		eq(ARRU.getInt(0), (int)0xcafebabe12345678l);
		
		try {
			ARRS.getInt(2);  fail();
		} catch(IndexOutOfBoundsException e) {}
		
		try {
			ARRU.getInt(-1);  fail();
		} catch(IndexOutOfBoundsException e) {}
	}
	
	public void testGetLong() {
		eq(ARRS.getLong(0), 0xcafebabe12345678l);
		eq(ARRU.getLong(0), 0xcafebabe12345678l);
	}
	
	public void testGetShort() {
		eq(ARRS.getShort(0), (short)0xcafebabe12345678l);
		eq(ARRU.getShort(0), (short)0xcafebabe12345678l);
	}
	
	public void testSetByte() {
		LongArray s = new LongArray(ARRS);
		LongArray u = new LongArray(ARRU);
		long[] r1 = new long[] {
				(byte)0xaa
		};
		long[] r2 = new long[] {
				0xaa
		};
		
		s.setByte(0, (byte)0xaa);
		u.setByte(0, (byte)0xaa);
		ok(Arrays.equals(r1, s.toArray()));
		ok(Arrays.equals(r2, u.toArray()));
	}
	
	public void testSetDouble() {
		LongArray s = new LongArray(ARRS);
		LongArray u = new LongArray(ARRU);
		
		s.setDouble(0, 0xaaaaaaaaaaaaaaaal);
		u.setDouble(0,
				Bytes.ulongToDouble(0xaaaaaaaaaaaaaaaal));
		//eq(TEST1, s.toArray());
		//eq(TEST1, u.toArray());
	}
	
	public void testSetFloat() {
		LongArray s = new LongArray(ARRS);
		LongArray u = new LongArray(ARRU);
		
		s.setFloat(0, 0xaaaaaaaaaaaaaaaal);
		u.setFloat(0,
				Bytes.ulongToFloat(0xaaaaaaaaaaaaaaaal));
		//eq(TEST1, s.toArray());
		//eq(TEST1, u.toArray());
	}
	
	public void testSetInt() {
		LongArray s = new LongArray(ARRS);
		LongArray u = new LongArray(ARRU);
		long[] r1 = new long[] {
				(int)0xaaaaaaaa
		};
		long[] r2 = new long[] {
				0xaaaaaaaal
		};
				
		s.setInt(0, 0xaaaaaaaa);
		u.setInt(0, 0xaaaaaaaa);
		eq(r1, s.toArray());
		eq(r2, u.toArray());
	}
	
	public void testSetLong() {
		LongArray s = new LongArray(ARRS);
		LongArray u = new LongArray(ARRU);
		
		s.setLong(0, 0xaaaaaaaaaaaaaaaal);
		u.setLong(0, 0xaaaaaaaaaaaaaaaal);
		ok(Arrays.equals(TEST1, s.toArray()));
		ok(Arrays.equals(TEST1, u.toArray()));
		
		try {
			s.setLong(1, 0);  fail();
		} catch(IndexOutOfBoundsException e) {}
		
		try {
			u.setLong(-1, 0);  fail();
		} catch(IndexOutOfBoundsException e) {}
	}
	
	public void testSetShort() {
		LongArray s = new LongArray(ARRS);
		LongArray u = new LongArray(ARRU);
		long[] r1 = new long[] {
				(short)0xaaaa
		};
		long[] r2 = new long[] {
				0xaaaa
		};
		
		s.setShort(0, (short)0xaaaa);
		u.setShort(0, (short)0xaaaa);
		eq(r1, s.toArray());
		eq(r2, u.toArray());
	}
	
	public void testToByteArray() {
		byte[] rb = new byte[] {
				(byte)0xca, (byte)0xfe, (byte)0xba, (byte)0xbe,
				(byte)0x12, (byte)0x34, (byte)0x56, (byte)0x78
		};
		byte[] rl = new byte[] {
				(byte)0x78, (byte)0x56, (byte)0x34, (byte)0x12,
				(byte)0xbe, (byte)0xba, (byte)0xfe, (byte)0xca
		};
		
		eq(rb, ARRS.toByteArray(Endianness.BIG));
		eq(rl, ARRS.toByteArray(Endianness.LITTLE));
	}
	
}
