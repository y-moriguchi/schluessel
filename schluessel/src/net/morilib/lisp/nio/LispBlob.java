/*
 * Copyright 2009-2010 Yuichiro Moriguchi
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
package net.morilib.lisp.nio;

import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.util.Arrays;

import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.Environment;
import net.morilib.lisp.SRFI74Endianness;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.util.Bytes;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/06/11
 */
public class LispBlob extends Datum2 {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/06/11
	 */
	public static class MakeBlob extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Environment env,
				LispMessage mesg) {
//			return new LispBlob(ByteBuffer.allocate(
//					SubrUtils.getSmallIntegerExact(c1a, mesg)));
			byte[] a;
			int s = SubrUtils.getSmallInt(c1a, mesg);

			if(s < 0) {
				throw mesg.getError("err.require.int.nonnegative",
						c1a);
			}
			a = new byte[s];
			Arrays.fill(a, (byte)0);
			return new LispBlob(ByteBuffer.wrap(a));
		}

	}

	//
	private ByteBuffer buffer;
	private int start;

	//
	/*package*/ LispBlob(ByteBuffer buf) {
		this.buffer = buf;
		this.start  = buf.position();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<srfi-74 blob>");
	}

	/**
	 * @param i
	 * @return
	 */
	public byte get(int i) {
		if(i < 0) {
			throw new IndexOutOfBoundsException("" + i);
		}
		return buffer.get(i + start);
	}

	/**
	 * @return
	 */
	public int length() {
		return buffer.limit() - start;
	}

	/**
	 * @param i
	 * @param b
	 */
	public void put(int i, byte b) {
		if(i < 0) {
			throw new IndexOutOfBoundsException("" + i);
		}
		buffer.put(i + start, b);
	}

	/**
	 * @param c2a
	 */
	public void order(SRFI74Endianness.Endian c2a) {
		buffer.order(c2a.getByteOrder());
	}

	/**
	 * @param k
	 * @return
	 */
	public short getShort(int k) {
		if(k < 0) {
			throw new IndexOutOfBoundsException("" + k);
		}
		return buffer.getShort(k + start);
	}

	/**
	 * @param k 
	 * @param number
	 */
	public void putShort(int k, short number) {
		if(k < 0) {
			throw new IndexOutOfBoundsException("" + k);
		}
		buffer.putShort(k + start, number);
	}

	/**
	 * @param k
	 * @return
	 */
	public int getInt(int k) {
		if(k < 0) {
			throw new IndexOutOfBoundsException("" + k);
		}
		return buffer.getInt(k + start);
	}

	/**
	 * @param k
	 * @param number
	 */
	public void putInt(int k, int number) {
		if(k < 0) {
			throw new IndexOutOfBoundsException("" + k);
		}
		buffer.putInt(k + start, number);
	}

	/**
	 * @param k
	 * @return
	 */
	public long getLong(int k) {
		if(k < 0) {
			throw new IndexOutOfBoundsException("" + k);
		}
		return buffer.getLong(k + start);
	}

	/**
	 * @param k
	 * @param number
	 */
	public void putLong(int k, long number) {
		if(k < 0) {
			throw new IndexOutOfBoundsException("" + k);
		}
		buffer.putLong(k + start, number);
	}

	/**
	 * @param k
	 */
	public BigInteger getBigInteger(int s, int k) {
		byte[] a = new byte[s];
		int p = buffer.position();

		if(k < 0) {
			throw new IndexOutOfBoundsException("" + k);
		} else {
			try {
				buffer.position(k + start);
				buffer.get(a, 0, s);
				return new BigInteger(a);
			} finally {
				buffer.position(p);
			}
		}
	}

	/**
	 * @param s
	 * @param k
	 * @param x
	 */
	public void put(int s, int k, BigInteger x) {
		byte[] a;
		byte[] a2 = x.toByteArray();
		int p = buffer.position();

		try {
			buffer.position(k);
			if(s >= a2.length) {
				a = new byte[s];
				Arrays.fill(a, (byte)0);
				System.arraycopy(a2, 0, a, s - a2.length, a2.length);
				buffer.put(a, 0, s);
			} else {
				buffer.put(a2, a2.length - s, s);
			}
		} finally {
			buffer.position(p);
		}
	}

	/**
	 * @param c2a
	 * @return
	 */
	public Datum toSintList(SRFI74Endianness.Endian c2a, int s) {
		ConsListBuilder c = new ConsListBuilder();
		byte[] a = new byte[s];

		buffer.position(start);
		buffer.order(c2a.getByteOrder());
		while(buffer.remaining() >= s) {
			buffer.get(a);
			c.append(LispInteger.valueOf(new BigInteger(a)));
		}
		return c.get();
	}

	/**
	 * @return
	 */
	public Datum toOctetList() {
		ConsListBuilder b = new ConsListBuilder();

		for(int i = start; i < buffer.limit(); i++) {
			b.append(LispInteger.valueOf(Bytes.ubyteToInt(
					buffer.get(i))));
		}
		return b.get();
	}

	/**
	 * @param c2a
	 * @param s
	 * @return
	 */
	public Datum toUintList(SRFI74Endianness.Endian c2a, int s) {
		ConsListBuilder c = new ConsListBuilder();
		byte[] a = new byte[s + 1];

		a[0] = 0;
		buffer.position(0);
		buffer.order(c2a.getByteOrder());
		while(buffer.remaining() >= s) {
			buffer.get(a, 1, s);
			c.append(LispInteger.valueOf(new BigInteger(a)));
		}
		return c.get();
	}

	/**
	 * @param s
	 * @param k
	 * @return
	 */
	public BigInteger getUnsignedBigInteger(int s, int k) {
		byte[] a = new byte[s + 1];
		int p = buffer.position();

		try {
			buffer.position(k);
			buffer.get(a, 1, s);
			a[0] = 0;
			return new BigInteger(a);
		} finally {
			buffer.position(p);
		}
	}

	/**
	 * @param s
	 * @param k
	 * @param x
	 */
	public void putUnsigned(int s, int k, BigInteger x) {
		int p = buffer.position();
		byte[] a = x.toByteArray();

		try {
			buffer.position(k);
			if(a[0] == 0) {
				buffer.put(a, 1, s);
			} else {
				byte[] a2 = new byte[s];

				Arrays.fill(a, (byte)0);
				System.arraycopy(a2, 0, a, s - a2.length, a2.length);
				buffer.put(a, 0, s);
			}
		} finally {
			buffer.position(p);
		}
	}

	/**
	 * @param c2a
	 * @return
	 */
	public boolean equalTo(LispBlob c2a) {
		ByteBuffer b1 = buffer;
		ByteBuffer b2 = ((LispBlob)c2a).buffer;
		int s2 = ((LispBlob)c2a).start;

		if(b1.limit() != b2.limit()) {
			return false;
		} else {
			for(int i = 0; i < length(); i++) {
				if(b1.get(i + start) != b2.get(i + s2)) {
					return false;
				}
			}
			return true;
		}
	}

}
