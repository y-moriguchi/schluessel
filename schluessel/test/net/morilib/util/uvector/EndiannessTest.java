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

public class EndiannessTest extends TC {
	
	static final byte[] TEST0 = new byte[] {
		(byte)0xca, (byte)0xfe, (byte)0xba, (byte)0xbe,
		(byte)0x12, (byte)0x34, (byte)0x56, (byte)0x78,
	};
	
	public void testGetInstance() {
		Endianness.getInstance(
				"test",
				new int[] { 1, 0 },
				new int[] { 1, 2, 3, 0 },
				new int[] { 1, 2, 3, 4, 5, 6, 7, 0 });
		
		try {
			Endianness.getInstance(
					"test",
					null,
					new int[] { 1, 2, 3, 0 },
					new int[] { 1, 2, 3, 4, 5, 6, 7, 0 });
			fail();
		} catch(NullPointerException e) {}
		
		try {
			Endianness.getInstance(
					"test",
					new int[] { 1, 0, 2 },
					new int[] { 1, 2, 3, 0 },
					new int[] { 1, 2, 3, 4, 5, 6, 7, 0 });
			fail();
		} catch(IllegalArgumentException e) {}
		
		try {
			Endianness.getInstance(
					"test",
					new int[] { 1, 0 },
					new int[] { 1, 2, 3, 0, 4 },
					new int[] { 1, 2, 3, 4, 5, 6, 7, 0 });
			fail();
		} catch(IllegalArgumentException e) {}
		
		try {
			Endianness.getInstance(
					"test",
					new int[] { 1, 0 },
					new int[] { 1, 2, 3, 0 },
					new int[] { 1, 2, 3, 4, 5, 6, 7, 0, 8 });
			fail();
		} catch(IllegalArgumentException e) {}
		
		try {
			Endianness.getInstance(
					"test",
					new int[] { 1, 2 },
					new int[] { 1, 2, 3, 0 },
					new int[] { 1, 2, 3, 4, 5, 6, 7, 0 });
			fail();
		} catch(IndexOutOfBoundsException e) {}
		
		try {
			Endianness.getInstance(
					"test",
					new int[] { 1, 0 },
					new int[] { 1, 2, 3, 1 },
					new int[] { 1, 2, 3, 4, 5, 6, 7, 0 });
			fail();
		} catch(IllegalArgumentException e) {}
	}
	
	public void testReadShort() {
		Endianness b = Endianness.BIG;
		Endianness l = Endianness.LITTLE;
		
		eq(b.readShort(TEST0, 0), (short)0xcafe);
		eq(l.readShort(TEST0, 0), (short)0xfeca);
		eq(l.readShort(TEST0, 6), (short)0x7856);
		
		try {
			b.readShort(TEST0, 7);  fail();
		} catch(IndexOutOfBoundsException e) {}
	}
	
	public void testReadInt() {
		Endianness b = Endianness.BIG;
		Endianness l = Endianness.LITTLE;
		
		eq(b.readInt(TEST0, 0), 0xcafebabe);
		eq(l.readInt(TEST0, 0), 0xbebafeca);
		eq(l.readInt(TEST0, 4), 0x78563412);
		
		try {
			b.readInt(TEST0, 5);  fail();
		} catch(IndexOutOfBoundsException e) {}
	}
	
	public void testReadLong() {
		Endianness b = Endianness.BIG;
		Endianness l = Endianness.LITTLE;
		
		eq(b.readLong(TEST0, 0), 0xcafebabe12345678l);
		eq(l.readLong(TEST0, 0), 0x78563412bebafecal);
		
		try {
			b.readLong(TEST0, 1);  fail();
		} catch(IndexOutOfBoundsException e) {}
	}
	
	public void testReaduShort() {
		Endianness b = Endianness.BIG;
		Endianness l = Endianness.LITTLE;
		
		eq(b.readuShort(TEST0, 0), 0xcafe);
		eq(l.readuShort(TEST0, 0), 0xfeca);
		eq(l.readuShort(TEST0, 6), 0x7856);
		
		try {
			b.readuShort(TEST0, 7);  fail();
		} catch(IndexOutOfBoundsException e) {}
	}
	
	public void testReaduInt() {
		Endianness b = Endianness.BIG;
		Endianness l = Endianness.LITTLE;
		
		eq(b.readuInt(TEST0, 0), 0xcafebabel);
		eq(l.readuInt(TEST0, 0), 0xbebafecal);
		eq(l.readuInt(TEST0, 4), 0x78563412l);
		
		try {
			b.readuInt(TEST0, 5);  fail();
		} catch(IndexOutOfBoundsException e) {}
	}
	
	public void testWriteShort() {
		Endianness b = Endianness.BIG;
		Endianness l = Endianness.LITTLE;
		byte[] r = new byte[8];
		
		b.writeShort(r, 0, (short)0xcafe);
		b.writeShort(r, 2, (short)0xbabe);
		b.writeShort(r, 4, (short)0x1234);
		b.writeShort(r, 6, (short)0x5678);
		ok(Arrays.equals(r, TEST0));
		
		l.writeShort(r, 0, (short)0xfeca);
		l.writeShort(r, 2, (short)0xbeba);
		l.writeShort(r, 4, (short)0x3412);
		l.writeShort(r, 6, (short)0x7856);
		ok(Arrays.equals(r, TEST0));
		
		try {
			b.writeShort(r, 7, (short)0);  fail();
		} catch(IndexOutOfBoundsException e) {}
	}
	
	public void testWriteInt() {
		Endianness b = Endianness.BIG;
		Endianness l = Endianness.LITTLE;
		byte[] r = new byte[8];
		
		b.writeInt(r, 0, 0xcafebabe);
		b.writeInt(r, 4, 0x12345678);
		ok(Arrays.equals(r, TEST0));
		
		l.writeInt(r, 0, 0xbebafeca);
		l.writeInt(r, 4, 0x78563412);
		ok(Arrays.equals(r, TEST0));
		
		try {
			b.writeInt(r, 5, 0);  fail();
		} catch(IndexOutOfBoundsException e) {}
	}
	
	public void testWriteLong() {
		Endianness b = Endianness.BIG;
		Endianness l = Endianness.LITTLE;
		byte[] r = new byte[8];
		
		b.writeLong(r, 0, 0xcafebabe12345678l);
		ok(Arrays.equals(r, TEST0));
		
		l.writeLong(r, 0, 0x78563412bebafecal);
		ok(Arrays.equals(r, TEST0));
		
		try {
			b.writeLong(r, 1, 0);  fail();
		} catch(IndexOutOfBoundsException e) {}
	}
	
	public void testWriteuShort() {
		Endianness b = Endianness.BIG;
		Endianness l = Endianness.LITTLE;
		byte[] r = new byte[8];
		
		b.writeuShort(r, 0, 0xcafe);
		b.writeuShort(r, 2, 0xbabe);
		b.writeuShort(r, 4, 0x1234);
		b.writeuShort(r, 6, 0x5678);
		ok(Arrays.equals(r, TEST0));
		
		l.writeuShort(r, 0, 0xfeca);
		l.writeuShort(r, 2, 0xbeba);
		l.writeuShort(r, 4, 0x3412);
		l.writeuShort(r, 6, 0x7856);
		ok(Arrays.equals(r, TEST0));
		
		try {
			b.writeuShort(r, 7, 0);  fail();
		} catch(IndexOutOfBoundsException e) {}
	}
	
	public void testWriteuInt() {
		Endianness b = Endianness.BIG;
		Endianness l = Endianness.LITTLE;
		byte[] r = new byte[8];
		
		b.writeuInt(r, 0, 0xcafebabel);
		b.writeuInt(r, 4, 0x12345678l);
		ok(Arrays.equals(r, TEST0));
		
		l.writeuInt(r, 0, 0xbebafecal);
		l.writeuInt(r, 4, 0x78563412l);
		ok(Arrays.equals(r, TEST0));
		
		try {
			b.writeuInt(r, 5, 0);  fail();
		} catch(IndexOutOfBoundsException e) {}
	}
	
}
