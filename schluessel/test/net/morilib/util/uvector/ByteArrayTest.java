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

public class ByteArrayTest extends TC {
	
	static final byte[] TEST0 = new byte[] {
		(byte)0xca, (byte)0xfe, (byte)0xba, (byte)0xbe,
		(byte)0x12, (byte)0x34, (byte)0x56, (byte)0x78,
	};
	static final byte[] TEST1 = new byte[] {
		(byte)0xaa, (byte)0xfe, (byte)0xba, (byte)0xbe,
		(byte)0x12, (byte)0x34, (byte)0x56, (byte)0x78,
	};
	static final ByteArray ARRS = ByteArray.newArray(TEST0);
	static final ByteArray ARRU = ByteArray.newuArray(TEST0);
	
	public void testA() {
//		System.out.println(Float.intBitsToFloat(0xfe));
//		System.out.println(Float.intBitsToFloat((byte)0xfe));
//		System.out.println(Float.intBitsToFloat(0xcafe));
//		System.out.println(Float.intBitsToFloat((short)0xcafe));
//		System.out.println(Float.intBitsToFloat(0xcafebabe));
//		System.out.println(Float.intBitsToFloat(0xbebafeca));
//		System.out.println(Float.intBitsToFloat(0x12345678));
//		System.out.println(Float.intBitsToFloat(0x78563412));
//		System.out.println(Double.longBitsToDouble(0xfel));
//		System.out.println(Double.longBitsToDouble((long)(byte)0xfe));
//		System.out.println(Double.longBitsToDouble(0xcafel));
//		System.out.println(Double.longBitsToDouble((long)(short)0xcafe));
//		System.out.println(Double.longBitsToDouble(0xcafebabel));
//		System.out.println(Double.longBitsToDouble((long)0xcafebabe));
//		System.out.println(Double.longBitsToDouble(0xcafebabe12345678l));
//		System.out.println(Double.longBitsToDouble(0x78563412bebafecal));
	}
	
	public void testNewArray() {
		ByteArray.newArray(TEST0);
		
		try {
			ByteArray.newArray(null);  fail();
		} catch(NullPointerException e) {}
	}
	
	public void testNewuArray() {
		ByteArray.newArray(TEST0);
		
		try {
			ByteArray.newuArray(null);  fail();
		} catch(NullPointerException e) {}
	}
	
	public void testGetByte() {
		eq(ARRS.getByte(0), (byte)0xca);
		eq(ARRU.getByte(0), (byte)0xca);
		eq(ARRS.getByte(7), (byte)0x78);
		eq(ARRU.getByte(7), (byte)0x78);
		
		try {
			ARRS.getByte(8);  fail();
		} catch(IndexOutOfBoundsException e) {}
		
		try {
			ARRU.getByte(-1);  fail();
		} catch(IndexOutOfBoundsException e) {}
	}
	
	public void testGetDouble() {
		eq(ARRS.getDouble(1), (byte)0xfe);
		eq(ARRU.getDouble(1), 0xfe);
	}
	
	public void testGetFloat() {
		eq(ARRS.getFloat(1), (byte)0xfe);
		eq(ARRU.getFloat(1), 0xfe);
	}
	
	public void testGetInt() {
		eq(ARRS.getInt(1), (byte)0xfe);
		eq(ARRU.getInt(1), 0xfe);
	}
	
	public void testGetLong() {
		eq(ARRS.getLong(1), (byte)0xfe);
		eq(ARRU.getLong(1), 0xfe);
	}
	
	public void testGetShort() {
		eq(ARRS.getShort(1), (byte)0xfe);
		eq(ARRU.getShort(1), 0xfe);
	}
	
	public void testSetByte() {
		ByteArray s = new ByteArray(ARRS);
		ByteArray u = new ByteArray(ARRU);
		
		s.setByte(0, (byte)0xaa);
		u.setByte(0, (byte)0xaa);
		ok(Arrays.equals(TEST1, s.toByteArray()));
		ok(Arrays.equals(TEST1, u.toByteArray()));
		
		try {
			s.getByte(8);  fail();
		} catch(IndexOutOfBoundsException e) {}
		
		try {
			u.getByte(-1);  fail();
		} catch(IndexOutOfBoundsException e) {}
	}
	
	public void testSetDouble() {
		ByteArray s = new ByteArray(ARRS);
		ByteArray u = new ByteArray(ARRU);
		
		s.setDouble(0, (byte)0xaa);
		u.setDouble(0, 0xaa);
		ok(Arrays.equals(TEST1, s.toByteArray()));
		ok(Arrays.equals(TEST1, u.toByteArray()));
	}
	
	public void testSetFloat() {
		ByteArray s = new ByteArray(ARRS);
		ByteArray u = new ByteArray(ARRU);
		
		s.setFloat(0, (byte)0xaa);
		u.setFloat(0, 0xaa);
		ok(Arrays.equals(TEST1, s.toByteArray()));
		ok(Arrays.equals(TEST1, u.toByteArray()));
	}
	
	public void testSetInt() {
		ByteArray s = new ByteArray(ARRS);
		ByteArray u = new ByteArray(ARRU);
		
		s.setInt(0, (byte)0xaa);
		u.setInt(0, 0xaa);
		ok(Arrays.equals(TEST1, s.toByteArray()));
		ok(Arrays.equals(TEST1, u.toByteArray()));
	}
	
	public void testSetLong() {
		ByteArray s = new ByteArray(ARRS);
		ByteArray u = new ByteArray(ARRU);
		
		s.setLong(0, (byte)0xaa);
		u.setLong(0, 0xaa);
		ok(Arrays.equals(TEST1, s.toByteArray()));
		ok(Arrays.equals(TEST1, u.toByteArray()));
	}
	
	public void testSetShort() {
		ByteArray s = new ByteArray(ARRS);
		ByteArray u = new ByteArray(ARRU);
		
		s.setShort(0, (byte)0xaa);
		u.setShort(0, (short)0xaa);
		ok(Arrays.equals(TEST1, s.toByteArray()));
		ok(Arrays.equals(TEST1, u.toByteArray()));
	}
	
	public void testReadByte() {
		eq(ARRS.readByte(0), (byte)0xca);
		eq(ARRS.readByte(7), (byte)0x78);
	}
	
	public void testReadDouble() {
		eq(ARRS.readDouble(0, Endianness.BIG),
				Double.longBitsToDouble(0xcafebabe12345678l));
		eq(ARRS.readDouble(0, Endianness.LITTLE),
				Double.longBitsToDouble(0x78563412bebafecal));
	}
	
	public void testReadFloat() {
		eq(ARRS.readFloat(0, Endianness.BIG),
				Float.intBitsToFloat(0xcafebabe));
		eq(ARRS.readFloat(0, Endianness.LITTLE),
				Float.intBitsToFloat(0xbebafeca));
	}
	
	public void testReadInt() {
		eq(ARRS.readInt(0, Endianness.BIG), 0xcafebabe);
		eq(ARRS.readInt(0, Endianness.LITTLE), 0xbebafeca);
	}
	
	public void testReadLong() {
		eq(ARRS.readLong(0, Endianness.BIG), 0xcafebabe12345678l);
		eq(ARRS.readLong(0, Endianness.LITTLE), 0x78563412bebafecal);
	}
	
	public void testReadShort() {
		eq(ARRS.readShort(0, Endianness.BIG), (short)0xcafe);
		eq(ARRS.readShort(0, Endianness.LITTLE), (short)0xfeca);
	}
	
	public void testWriteByte() {
		ByteArray s = new ByteArray(ARRS);
		byte[] rs = new byte[] {
				(byte)0xaa, (byte)0xfe, (byte)0xba, (byte)0xbe,
				(byte)0x12, (byte)0x34, (byte)0x56, (byte)0x78,
		};
		
		s.writeByte(0, (byte)0xaa);
		ok(Arrays.equals(rs, s.toByteArray()));
	}
	
	public void testWriteDouble() {
		ByteArray b = new ByteArray(ARRS);
		ByteArray l = new ByteArray(ARRS);
		byte[] r = new byte[] {
				(byte)0xaa, (byte)0xbb, (byte)0xcc, (byte)0xdd,
				(byte)0x11, (byte)0x22, (byte)0x33, (byte)0x44,
		};
		
		b.writeDouble(0, Double.longBitsToDouble(0xaabbccdd11223344l),
				Endianness.BIG);
		l.writeDouble(0, Double.longBitsToDouble(0x44332211ddccbbaal),
				Endianness.LITTLE);
		ok(Arrays.equals(r, b.toByteArray()));
		ok(Arrays.equals(r, l.toByteArray()));
	}
	
	public void testWriteFloat() {
		ByteArray b = new ByteArray(ARRS);
		ByteArray l = new ByteArray(ARRS);
		byte[] r = new byte[] {
				(byte)0xaa, (byte)0xbb, (byte)0xcc, (byte)0xdd,
				(byte)0x12, (byte)0x34, (byte)0x56, (byte)0x78,
		};
		
		b.writeFloat(0, Float.intBitsToFloat(0xaabbccdd),
				Endianness.BIG);
		l.writeFloat(0, Float.intBitsToFloat(0xddccbbaa),
				Endianness.LITTLE);
		ok(Arrays.equals(r, b.toByteArray()));
		ok(Arrays.equals(r, l.toByteArray()));
	}
	
	public void testWriteInt() {
		ByteArray b = new ByteArray(ARRS);
		ByteArray l = new ByteArray(ARRS);
		byte[] r = new byte[] {
				(byte)0xaa, (byte)0xbb, (byte)0xcc, (byte)0xdd,
				(byte)0x12, (byte)0x34, (byte)0x56, (byte)0x78,
		};
		
		b.writeInt(0, 0xaabbccdd, Endianness.BIG);
		l.writeInt(0, 0xddccbbaa, Endianness.LITTLE);
		ok(Arrays.equals(r, b.toByteArray()));
		ok(Arrays.equals(r, l.toByteArray()));
	}
	
	public void testWriteLong() {
		ByteArray b = new ByteArray(ARRS);
		ByteArray l = new ByteArray(ARRS);
		byte[] r = new byte[] {
				(byte)0xaa, (byte)0xbb, (byte)0xcc, (byte)0xdd,
				(byte)0x11, (byte)0x22, (byte)0x33, (byte)0x44,
		};
		
		b.writeLong(0, 0xaabbccdd11223344l, Endianness.BIG);
		l.writeLong(0, 0x44332211ddccbbaal, Endianness.LITTLE);
		ok(Arrays.equals(r, b.toByteArray()));
		ok(Arrays.equals(r, l.toByteArray()));
	}
	
	public void testWriteShort() {
		ByteArray b = new ByteArray(ARRS);
		ByteArray l = new ByteArray(ARRS);
		byte[] r = new byte[] {
				(byte)0xaa, (byte)0xbb, (byte)0xba, (byte)0xbe,
				(byte)0x12, (byte)0x34, (byte)0x56, (byte)0x78,
		};
		
		b.writeShort(0, (short)0xaabb, Endianness.BIG);
		l.writeShort(0, (short)0xbbaa, Endianness.LITTLE);
		ok(Arrays.equals(r, b.toByteArray()));
		ok(Arrays.equals(r, l.toByteArray()));
	}
	
	public void testReaduByte() {
		eq(ARRS.readuByte(0), 0xca);
		eq(ARRS.readuByte(7), 0x78);
	}
	
	public void testReaduInt() {
		eq(ARRS.readuInt(0, Endianness.BIG), 0xcafebabel);
		eq(ARRS.readuInt(0, Endianness.LITTLE), 0xbebafecal);
	}
	
	public void testReaduShort() {
		eq(ARRS.readuShort(0, Endianness.BIG), 0xcafe);
		eq(ARRS.readuShort(0, Endianness.LITTLE), 0xfeca);
	}
	
	public void testWriteuByte() {
		ByteArray s = new ByteArray(ARRS);
		byte[] rs = new byte[] {
				(byte)0xaa, (byte)0xfe, (byte)0xba, (byte)0xbe,
				(byte)0x12, (byte)0x34, (byte)0x56, (byte)0x78,
		};
		
		s.writeuByte(0, 0xaa);
		ok(Arrays.equals(rs, s.toByteArray()));
	}
	
	public void testWriteuInt() {
		ByteArray b = new ByteArray(ARRS);
		ByteArray l = new ByteArray(ARRS);
		byte[] r = new byte[] {
				(byte)0xaa, (byte)0xbb, (byte)0xcc, (byte)0xdd,
				(byte)0x12, (byte)0x34, (byte)0x56, (byte)0x78,
		};
		
		b.writeuInt(0, 0xaabbccddl, Endianness.BIG);
		l.writeuInt(0, 0xddccbbaal, Endianness.LITTLE);
		ok(Arrays.equals(r, b.toByteArray()));
		ok(Arrays.equals(r, l.toByteArray()));
	}
	
	public void testWriteuShort() {
		ByteArray b = new ByteArray(ARRS);
		ByteArray l = new ByteArray(ARRS);
		byte[] r = new byte[] {
				(byte)0xaa, (byte)0xbb, (byte)0xba, (byte)0xbe,
				(byte)0x12, (byte)0x34, (byte)0x56, (byte)0x78,
		};
		
		b.writeuShort(0, 0xaabb, Endianness.BIG);
		l.writeuShort(0, 0xbbaa, Endianness.LITTLE);
		ok(Arrays.equals(r, b.toByteArray()));
		ok(Arrays.equals(r, l.toByteArray()));
	}
	
	public void testReadDoubleB() {
		eq(ARRS.readDoubleB(0),
				Double.longBitsToDouble(0xcafebabe12345678l));
	}
	
	public void testReadFloatB() {
		eq(ARRS.readFloatB(0),
				Float.intBitsToFloat(0xcafebabe));
	}
	
	public void testReadIntB() {
		eq(ARRS.readIntB(0), 0xcafebabe);
	}
	
	public void testReadLongB() {
		eq(ARRS.readLongB(0), 0xcafebabe12345678l);
	}
	
	public void testReadShortB() {
		eq(ARRS.readShortB(0), (short)0xcafe);
	}
	
	public void testWriteDoubleB() {
		ByteArray b = new ByteArray(ARRS);
		byte[] r = new byte[] {
				(byte)0xaa, (byte)0xbb, (byte)0xcc, (byte)0xdd,
				(byte)0x11, (byte)0x22, (byte)0x33, (byte)0x44,
		};
		
		b.writeDoubleB(0, Double.longBitsToDouble(0xaabbccdd11223344l));
		ok(Arrays.equals(r, b.toByteArray()));
	}
	
	public void testWriteFloatB() {
		ByteArray b = new ByteArray(ARRS);
		byte[] r = new byte[] {
				(byte)0xaa, (byte)0xbb, (byte)0xcc, (byte)0xdd,
				(byte)0x12, (byte)0x34, (byte)0x56, (byte)0x78,
		};
		
		b.writeFloatB(0, Float.intBitsToFloat(0xaabbccdd));
		ok(Arrays.equals(r, b.toByteArray()));
	}
	
	public void testWriteIntB() {
		ByteArray b = new ByteArray(ARRS);
		byte[] r = new byte[] {
				(byte)0xaa, (byte)0xbb, (byte)0xcc, (byte)0xdd,
				(byte)0x12, (byte)0x34, (byte)0x56, (byte)0x78,
		};
		
		b.writeIntB(0, 0xaabbccdd);
		ok(Arrays.equals(r, b.toByteArray()));
	}
	
	public void testWriteLongB() {
		ByteArray b = new ByteArray(ARRS);
		byte[] r = new byte[] {
				(byte)0xaa, (byte)0xbb, (byte)0xcc, (byte)0xdd,
				(byte)0x11, (byte)0x22, (byte)0x33, (byte)0x44,
		};
		
		b.writeLongB(0, 0xaabbccdd11223344l);
		ok(Arrays.equals(r, b.toByteArray()));
	}
	
	public void testWriteShortB() {
		ByteArray b = new ByteArray(ARRS);
		byte[] r = new byte[] {
				(byte)0xaa, (byte)0xbb, (byte)0xba, (byte)0xbe,
				(byte)0x12, (byte)0x34, (byte)0x56, (byte)0x78,
		};
		
		b.writeShortB(0, (short)0xaabb);
		ok(Arrays.equals(r, b.toByteArray()));
	}
	
	public void testReadDoubleL() {
		eq(ARRS.readDoubleL(0),
				Double.longBitsToDouble(0x78563412bebafecal));
	}
	
	public void testReadFloatL() {
		eq(ARRS.readFloatL(0),
				Float.intBitsToFloat(0xbebafeca));
	}
	
	public void testReadIntL() {
		eq(ARRS.readIntL(0), 0xbebafeca);
	}
	
	public void testReadLongL() {
		eq(ARRS.readLongL(0), 0x78563412bebafecal);
	}
	
	public void testReadShortL() {
		eq(ARRS.readShortL(0), (short)0xfeca);
	}
	
	public void testWriteDoubleL() {
		ByteArray l = new ByteArray(ARRS);
		byte[] r = new byte[] {
				(byte)0xaa, (byte)0xbb, (byte)0xcc, (byte)0xdd,
				(byte)0x11, (byte)0x22, (byte)0x33, (byte)0x44,
		};
		
		l.writeDoubleL(0, Double.longBitsToDouble(0x44332211ddccbbaal));
		ok(Arrays.equals(r, l.toByteArray()));
	}
	
	public void testWriteFloatL() {
		ByteArray l = new ByteArray(ARRS);
		byte[] r = new byte[] {
				(byte)0xaa, (byte)0xbb, (byte)0xcc, (byte)0xdd,
				(byte)0x12, (byte)0x34, (byte)0x56, (byte)0x78,
		};
		
		l.writeFloatL(0, Float.intBitsToFloat(0xddccbbaa));
		ok(Arrays.equals(r, l.toByteArray()));
	}
	
	public void testWriteIntL() {
		ByteArray l = new ByteArray(ARRS);
		byte[] r = new byte[] {
				(byte)0xaa, (byte)0xbb, (byte)0xcc, (byte)0xdd,
				(byte)0x12, (byte)0x34, (byte)0x56, (byte)0x78,
		};
		
		l.writeIntL(0, 0xddccbbaa);
		ok(Arrays.equals(r, l.toByteArray()));
	}
	
	public void testWriteLongL() {
		ByteArray l = new ByteArray(ARRS);
		byte[] r = new byte[] {
				(byte)0xaa, (byte)0xbb, (byte)0xcc, (byte)0xdd,
				(byte)0x11, (byte)0x22, (byte)0x33, (byte)0x44,
		};
		
		l.writeLongL(0, 0x44332211ddccbbaal);
		ok(Arrays.equals(r, l.toByteArray()));
	}
	
	public void testWriteShortL() {
		ByteArray l = new ByteArray(ARRS);
		byte[] r = new byte[] {
				(byte)0xaa, (byte)0xbb, (byte)0xba, (byte)0xbe,
				(byte)0x12, (byte)0x34, (byte)0x56, (byte)0x78,
		};
		
		l.writeShortL(0, (short)0xbbaa);
		ok(Arrays.equals(r, l.toByteArray()));
	}
	
	public void testReaduIntB() {
		eq(ARRS.readuIntB(0), 0xcafebabel);
	}
	
	public void testReaduShortB() {
		eq(ARRS.readuShortB(0), 0xcafe);
	}
	
	public void testWriteuIntB() {
		ByteArray b = new ByteArray(ARRS);
		byte[] r = new byte[] {
				(byte)0xaa, (byte)0xbb, (byte)0xcc, (byte)0xdd,
				(byte)0x12, (byte)0x34, (byte)0x56, (byte)0x78,
		};
		
		b.writeuIntB(0, 0xaabbccddl);
		ok(Arrays.equals(r, b.toByteArray()));
	}
	
	public void testWriteuShortB() {
		ByteArray b = new ByteArray(ARRS);
		byte[] r = new byte[] {
				(byte)0xaa, (byte)0xbb, (byte)0xba, (byte)0xbe,
				(byte)0x12, (byte)0x34, (byte)0x56, (byte)0x78,
		};
		
		b.writeuShortB(0, 0xaabb);
		ok(Arrays.equals(r, b.toByteArray()));
	}
	
	public void testReaduIntL() {
		eq(ARRS.readuIntL(0), 0xbebafecal);
	}
	
	public void testReaduShortL() {
		eq(ARRS.readuShortL(0), 0xfeca);
	}
	
	public void testWriteuIntL() {
		ByteArray l = new ByteArray(ARRS);
		byte[] r = new byte[] {
				(byte)0xaa, (byte)0xbb, (byte)0xcc, (byte)0xdd,
				(byte)0x12, (byte)0x34, (byte)0x56, (byte)0x78,
		};
		
		l.writeuIntL(0, 0xddccbbaal);
		ok(Arrays.equals(r, l.toByteArray()));
	}
	
	public void testWriteuShortL() {
		ByteArray l = new ByteArray(ARRS);
		byte[] r = new byte[] {
				(byte)0xaa, (byte)0xbb, (byte)0xba, (byte)0xbe,
				(byte)0x12, (byte)0x34, (byte)0x56, (byte)0x78,
		};
		
		l.writeuShortL(0, 0xbbaa);
		ok(Arrays.equals(r, l.toByteArray()));
	}
	
}
