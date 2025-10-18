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
package net.morilib.util;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2010
 */
public final class BitUtils {

	//
	private BitUtils() { }

	//
	private static int _countBit(long x, int s, int e) {
		if(s == e) {
			return (x & (1 << s)) != 0 ? 1 : 0;
		} else {
			int c = (e - s) / 2 + s;

			return (_countBit(x, s, c) + _countBit(x, c + 1, e));
		}
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int countBit(int x) {
		return _countBit(x, 0, 31);
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int countBit(long x) {
		return _countBit(x, 0, 63);
	}

	//
	private static int _getMsb(long x, int s, int e) {
		if(s == e) {
			return s;
		} else {
			int c = ((e - s) >> 1) + s;
			int m = ((1 << (((e - s) >> 1) + 1)) - 1) << c;

			if((x & m) != 0) {
				return _getMsb(x, c + 1, e);
			} else {
				return _getMsb(x, s, c);
			}
		}
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int getMsb(int x) {
		return (x == 0) ? 0 : _getMsb(x, 0, 31);
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int getMsb(long x) {
		return (x == 0) ? 0 : _getMsb(x, 0, 63);
	}

	//
	private static int _getLsb(long x, int s, int e) {
		if(s == e) {
			return s + 1;
		} else {
			int c = ((e - s) >> 1) + s;
			int m = ((1 << (((e - s) >> 1) + 1)) - 1) << s;

			if((x & m) != 0) {
				return _getLsb(x, s, c);
			} else {
				return _getLsb(x, c + 1, e);
			}
		}
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int getLsb(int x) {
		return (x == 0) ? 0 : _getLsb(x, 0, 31);
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int getLsb(long x) {
		return (x == 0) ? 0 : _getLsb(x, 0, 63);
	}

	/**
	 * 
	 * @param bits
	 * @return
	 */
	public static long getMask(int bits) {
		if(bits < 0 || bits > 64) {
			throw new IllegalArgumentException();
		} else if(bits == 0) {
			return 0;
		} else if(bits == 64) {
			return -1l;
		} else if(bits == 63) {
			return 0x7fffffffffffffffl;
		} else {
			return (1l << (bits + 1)) - 1;
		}
	}

	/**
	 * 
	 * @param bits
	 * @return
	 */
	public static int getIntMask(int bits) {
		if(bits < 0 || bits > 32) {
			throw new IllegalArgumentException();
		} else if(bits == 0) {
			return 0;
		} else if(bits == 32) {
			return -1;
		} else if(bits == 31) {
			return 0x7fffffff;
		} else {
			return (1 << (bits + 1)) - 1;
		}
	}

	/**
	 * 
	 * @param bits
	 * @return
	 */
	public static short getShortMask(int bits) {
		if(bits < 0 || bits > 16) {
			throw new IllegalArgumentException();
		} else if(bits == 0) {
			return 0;
		} else if(bits == 16) {
			return -1;
		} else if(bits == 15) {
			return 0x7fff;
		} else {
			return (short)((1 << (bits + 1)) - 1);
		}
	}

	/**
	 * 
	 * @param bits
	 * @return
	 */
	public static byte getByteMask(int bits) {
		if(bits < 0 || bits > 8) {
			throw new IllegalArgumentException();
		} else if(bits == 0) {
			return 0;
		} else if(bits == 8) {
			return -1;
		} else if(bits == 7) {
			return 0x7f;
		} else {
			return (byte)((1 << (bits + 1)) - 1);
		}
	}

	/**
	 * 
	 * @param x
	 * @param start
	 * @param end
	 * @param bits
	 * @return
	 */
	public static int rotate(int x, int start, int end, int bits) {
		int w = end - start, r, m, s;

		if(end < 0 || end >= 32) {
			throw new IllegalArgumentException();
		} else if(start < 0 || start > end) {
			throw new IllegalArgumentException();
		} else if(bits < 0 || bits > w) {
			throw new IllegalArgumentException();
		}
		m = ~(-1 << w) << start;
		r = x & ~m;
		s = x & m;
		s = ((s << bits) | (s >> (w - bits))) & m;
		return s | r;
	}

	/**
	 * 
	 * @param x
	 * @param start
	 * @param end
	 * @return
	 */
	public static int reverse(int x, int start, int end) {
		int w = end - start, r = x, a, b, j, k;

		if(end < 0 || end >= 32) {
			throw new IllegalArgumentException();
		} else if(start < 0 || start > end) {
			throw new IllegalArgumentException();
		}

		for(int i = 0; i < w / 2; i++) {
			j = i + start;  k = end - i - 1;
			a = r & (1 << j);
			b = r & (1 << k);
			r = (b != 0) ? (r | (1 << j)) : (r & ~(1 << j));
			r = (a != 0) ? (r | (1 << k)) : (r & ~(1 << k));
		}
		return r;
	}

}
