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
package net.morilib.lang.string;

import java.util.Arrays;

import net.morilib.lang.Hashes;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/10/10
 */
public class ByteString extends GenericString<ByteString>
implements ByteSequence, java.io.Serializable {

	//
	private static final long serialVersionUID = 5378573669505615859L;

	/**
	 * 
	 */
	public static final ByteCharTable ASCII = new ByteCharTable() {

		public byte toByte(char c) {
			return (c >= 0 && c <= 127) ? (byte)c : (byte)'?';
		}

		public char toChar(byte b) {
			return (b >= 0 && b <= 127) ? (char)b : '?';
		}

	};

	//
	private static final ByteString TRUE_S  = new ByteString("true");
	private static final ByteString FALSE_S = new ByteString("false");

	/**
	 * 
	 */
	protected byte[] bytes;

	/**
	 * 
	 */
	public ByteString() {
		bytes = new byte[0];
	}

	/**
	 * 
	 * @param bytes
	 * @param d
	 */
	protected ByteString(byte[] bytes, boolean d) {
		this.bytes = bytes;
	}

	/**
	 * 
	 * @param bytes
	 */
	public ByteString(byte[] bytes) {
		this(bytes, 0, bytes.length);
	}

	/**
	 * 
	 * @param value
	 * @param offset
	 * @param count
	 */
	public ByteString(byte[] value, int offset, int count) {
		if(offset < 0) {
			throw new IllegalArgumentException();
		} else if(count < 0) {
			throw new IllegalArgumentException();
		} else if(offset + count > value.length) {
			throw new IllegalArgumentException();
		}

		this.bytes = new byte[count];
		System.arraycopy(value, offset, this.bytes, 0, count);
	}

	/**
	 * 
	 * @param s
	 */
	public ByteString(ByteString s) {
		this(s.bytes);
	}

	/**
	 * 
	 * @param s
	 */
	public ByteString(ByteSequence s) {
		bytes = new byte[s.length()];
		for(int i = 0; i < s.length(); i++) {
			bytes[i] = s.byteAt(i);
		}
	}

	/**
	 * 
	 * @param s
	 * @param mp
	 */
	public ByteString(String s, ByteCharTable mp) {
		bytes = new byte[s.length()];
		for(int i = 0; i < s.length(); i++) {
			bytes[i] = mp.toByte(s.charAt(i));
		}
	}

	/**
	 * 
	 * @param s
	 */
	public ByteString(String s) {
		this(s, ASCII);
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static ByteString valueOf(String s, ByteCharTable mp) {
		return new ByteString(s, mp);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.string.GenericString#newArray(int)
	 */
	@Override
	protected Object newArray(int size) {
		return new byte[size];
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.string.GenericString#getArray()
	 */
	@Override
	protected Object getArray() {
		return bytes;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.string.GenericString#returnString(java.lang.Object)
	 */
	@Override
	protected ByteString returnString(Object a) {
		return new ByteString((byte[])a, false);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.string.GenericString#equalsAt(int, byte)
	 */
	@Override
	protected boolean equalsAt(int index, byte b) {
		return bytes[index] == b;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.string.GenericString#equalsAt(int, net.morilib.lang.string.GenericString, int)
	 */
	@Override
	protected boolean equalsAt(int index, ByteString b, int indexB) {
		return bytes[index] == b.bytes[indexB];
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.string.GenericString#copyByteArray()
	 */
	@Override
	protected byte[] copyByteArray() {
		byte[] res = new byte[bytes.length];

		System.arraycopy(bytes, 0, res, 0, bytes.length);
		return res;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.ByteSequence#byteAt(int)
	 */
	public byte byteAt(int index) {
		return bytes[index];
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.ByteSequence#length()
	 */
	@Override
	public int length() {
		return bytes.length;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.ByteSequence#subSequence()
	 */
	public ByteSequence subSequence(int beginIndex, int endIndex) {
		return substring(beginIndex, endIndex);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.ByteSequence#toByteString()
	 */
	public ByteString toByteString() {
		return this;
	}

	/**
	 * 
	 * @param s
	 * @param mp
	 * @return
	 */
	public ByteString concat(String s, ByteCharTable mp) {
		return concat(valueOf(s, mp));
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public ByteString concat(String s) {
		return concat(valueOf(s, ASCII));
	}

	/**
	 * 
	 * @param s
	 * @param mp
	 * @return
	 */
	public boolean contains(String s, ByteCharTable mp) {
		return contains(valueOf(s, mp));
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public boolean contains(String s) {
		return contains(valueOf(s, ASCII));
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public boolean containsEquals(ByteSequence s) {
		return contains(new ByteString(s));
	}

	/**
	 * 
	 * @param s
	 * @param mp
	 * @return
	 */
	public boolean containsEquals(String s, ByteCharTable mp) {
		return containsEquals(valueOf(s, mp));
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public boolean containsEquals(String s) {
		return containsEquals(valueOf(s, ASCII));
	}

	/**
	 * 
	 * @param b
	 * @return
	 */
	public static ByteString copyValueOf(byte[] b) {
		return copyValueOf(b, 0, b.length);
	}

	/**
	 * 
	 * @param b
	 * @param offset
	 * @param count
	 * @return
	 */
	public static ByteString copyValueOf(
			byte[] b, int offset, int count) {
		return new ByteString(b, offset, count);
	}

	/**
	 * 
	 * @param b
	 * @param mp
	 * @return
	 */
	public boolean endsWith(String b, ByteCharTable mp) {
		return endsWith(valueOf(b, mp));
	}

	/**
	 * 
	 * @param b
	 * @return
	 */
	public boolean endsWith(String b) {
		return endsWith(valueOf(b, ASCII));
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		if(o instanceof ByteString) {
			return Arrays.equals(bytes, ((ByteString)o).bytes);
		}
		return false;
	}

	/**
	 * 
	 * @param s
	 * @param mp
	 * @return
	 */
	public boolean equalsString(String s, ByteCharTable mp) {
		return equals(valueOf(s, mp));
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public boolean equalsString(String s) {
		return equals(valueOf(s, ASCII));
	}

	/**
	 * 
	 * @return
	 */
	public byte[] getBytes() {
		byte[] res = new byte[bytes.length];

		System.arraycopy(bytes, 0, res, 0, bytes.length);
		return res;
	}

	/**
	 * 
	 * @param srcBegin
	 * @param srcEnd
	 * @param dst
	 * @param dstBegin
	 */
	public void getBytes(
			int srcBegin, int srcEnd, byte[] dst, int dstBegin) {
		if(srcBegin < 0) {
			throw new IllegalArgumentException();
		} else if(srcEnd < 0) {
			throw new IllegalArgumentException();
		} else if(srcEnd > bytes.length) {
			throw new IllegalArgumentException();
		} else if(dstBegin < 0) {
			throw new IllegalArgumentException();
		} else if(dstBegin + (srcEnd - srcBegin) > dst.length) {
			throw new IllegalArgumentException();
		}

		System.arraycopy(
				bytes, srcBegin, dst, dstBegin, srcEnd - srcBegin);
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return Hashes.sumHashCode(bytes);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.string.GenericString#indexOf(int)
	 */
	public int indexOf(int ch) {
		return indexOf(ch, 0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.string.GenericString#indexOf(int, int)
	 */
	public int indexOf(int ch, int fromIndex) {
		byte b;

		if(ch < Byte.MIN_VALUE && ch > 255) {
			throw new IllegalArgumentException();
		} else if(ch > Byte.MAX_VALUE) {
			b = (byte)(ch - 256);
		} else {
			b = (byte)ch;
		}
		return super.indexOf(b, fromIndex);
	}

	/**
	 * 
	 * @param s
	 * @param mp
	 * @return
	 */
	public int indexOf(String s, ByteCharTable mp) {
		return indexOf(valueOf(s, mp), 0);
	}

	/**
	 * 
	 * @param s
	 * @param fromIndex
	 * @param mp
	 * @return
	 */
	public int indexOf(String s, int fromIndex, ByteCharTable mp) {
		return indexOf(valueOf(s, mp), fromIndex);
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public int indexOf(String s) {
		return indexOf(valueOf(s, ASCII), 0);
	}

	/**
	 * 
	 * @param s
	 * @param fromIndex
	 * @return
	 */
	public int indexOf(String s, int fromIndex) {
		return indexOf(valueOf(s, ASCII), fromIndex);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.string.GenericString#lastIndexOf(int)
	 */
	public int lastIndexOf(int ch) {
		return lastIndexOf(ch, 0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.string.GenericString#lastIndexOf(int, int)
	 */
	public int lastIndexOf(int ch, int fromIndex) {
		byte b;

		if(ch < Byte.MIN_VALUE && ch > 255) {
			throw new IllegalArgumentException();
		} else if(ch > Byte.MAX_VALUE) {
			b = (byte)(ch - 256);
		} else {
			b = (byte)ch;
		}
		return super.lastIndexOf(b, fromIndex);
	}

	/**
	 * 
	 * @param s
	 * @param mp
	 * @return
	 */
	public int lastIndexOf(String s, ByteCharTable mp) {
		return lastIndexOf(valueOf(s, mp), 0);
	}

	/**
	 * 
	 * @param s
	 * @param fromIndex
	 * @param mp
	 * @return
	 */
	public int lastIndexOf(String s, int fromIndex, ByteCharTable mp) {
		return lastIndexOf(valueOf(s, mp), fromIndex);
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public int lastIndexOf(String s) {
		return lastIndexOf(valueOf(s, ASCII), 0);
	}

	/**
	 * 
	 * @param s
	 * @param fromIndex
	 * @return
	 */
	public int lastIndexOf(String s, int fromIndex) {
		return lastIndexOf(valueOf(s, ASCII), fromIndex);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.string.GenericString#replace(byte, byte)
	 */
	public ByteString replace(byte oldChar, byte newChar) {
		return super.replace(oldChar, newChar);
	}

	/**
	 * 
	 * @param b
	 * @param mp
	 * @return
	 */
	public boolean startsWith(String b, ByteCharTable mp) {
		return startsWith(valueOf(b, mp));
	}

	/**
	 * 
	 * @param b
	 * @return
	 */
	public boolean startsWith(String b) {
		return startsWith(valueOf(b, ASCII));
	}

	/**
	 * 
	 * @param v
	 * @return
	 */
	public static ByteString valueOf(boolean v) {
		return v ? TRUE_S : FALSE_S;
	}

	/**
	 * 
	 * @param v
	 * @return
	 */
	public static ByteString valueOf(byte v) {
		byte[] res = new byte[1];

		res[0] = v;
		return new ByteString(res, false);
	}

	/**
	 * 
	 * @param v
	 * @return
	 */
	public static ByteString valueOf(double v) {
		return valueOf(String.valueOf(v), ASCII);
	}

	/**
	 * 
	 * @param v
	 * @return
	 */
	public static ByteString valueOf(float v) {
		return valueOf(String.valueOf(v), ASCII);
	}

	/**
	 * 
	 * @param v
	 * @return
	 */
	public static ByteString valueOf(int v) {
		return valueOf(String.valueOf(v), ASCII);
	}

	/**
	 * 
	 * @param v
	 * @return
	 */
	public static ByteString valueOf(long v) {
		return valueOf(String.valueOf(v), ASCII);
	}

	/**
	 * 
	 * @param t
	 * @return
	 */
	public String toString(ByteCharTable t) {
		StringBuffer b = new StringBuffer();

		for(int i = 0; i < bytes.length; i++) {
			b.append(t.toChar(bytes[i]));
		}
		return b.toString();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return toString(ASCII);
	}

}
