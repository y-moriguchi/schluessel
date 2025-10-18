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

import net.morilib.util.Objects;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/10/10
 */
public abstract class GenericString<S extends GenericString<S>> {

	
	public abstract int length();

	
	protected abstract Object newArray(int size);

	
	protected abstract Object getArray();

	
	protected abstract S returnString(Object a);

	
	protected S returnString(byte[] a) {
		throw new RuntimeException();
	}

	
	protected S returnString(char[] a) {
		throw new RuntimeException();
	}

	
	protected S returnString(short[] a) {
		throw new RuntimeException();
	}

	
	protected S returnString(int[] a) {
		throw new RuntimeException();
	}

	
	protected S returnString(long[] a) {
		throw new RuntimeException();
	}

	
	protected S returnString(double[] a) {
		throw new RuntimeException();
	}

	
	protected S returnString(float[] a) {
		throw new RuntimeException();
	}

	
	protected boolean equalsAt(int index, byte b) {
		throw new RuntimeException();
	}

	
	protected boolean equalsAt(int index, char b) {
		throw new RuntimeException();
	}

	
	protected boolean equalsAt(int index, short b) {
		throw new RuntimeException();
	}

	
	protected boolean equalsAt(int index, int b) {
		throw new RuntimeException();
	}

	
	protected boolean equalsAt(int index, long b) {
		throw new RuntimeException();
	}

	
	protected boolean equalsAt(int index, double b) {
		throw new RuntimeException();
	}

	
	protected boolean equalsAt(int index, float b) {
		throw new RuntimeException();
	}

	
	protected boolean equalsAt(int index, Object b) {
		throw new RuntimeException();
	}

	
	protected abstract boolean equalsAt(int index, S b, int indexB);
	
	
	protected byte[] copyByteArray() {
		throw new RuntimeException();
	}
	
	
	protected char[] copyCharArray() {
		throw new RuntimeException();
	}
	
	
	protected short[] copyShortArray() {
		throw new RuntimeException();
	}
	
	
	protected int[] copyIntArray() {
		throw new RuntimeException();
	}
	
	
	protected long[] copyLongArray() {
		throw new RuntimeException();
	}
	
	
	protected double[] copyDoubleArray() {
		throw new RuntimeException();
	}
	
	
	protected float[] copyFloatArray() {
		throw new RuntimeException();
	}
	
	
	protected Object[] copyObjectArray() {
		throw new RuntimeException();
	}
	
	
	public S concat(S s) {
		Object res = newArray(length() + s.length());
		
		System.arraycopy(getArray(), 0, res, 0, length());
		System.arraycopy(getArray(), length(), res, 0, s.length());
		return returnString(res);
	}
	
	
	public boolean contains(S s) {
		for(int i = 0; i <= length() - s.length(); i++) {
			if(indexOf(s, i) >= 0) {
				return true;
			}
		}
		return false;
	}
	
	
	public boolean endsWith(S b) {
		if(b.length() > length()) {
			return false;
		}
		
		for(int i = 0; i < b.length(); i++) {
			if(!equalsAt(length() - i - 1, b, b.length() - i - 1)) {
				return false;
			}
		}
		return true;
	}
	
	
	protected int indexOf(byte ch) {
		return indexOf(ch, 0);
	}
	
	
	protected int indexOf(char ch) {
		return indexOf(ch, 0);
	}
	
	
	protected int indexOf(short ch) {
		return indexOf(ch, 0);
	}
	
	
	protected int indexOf(int ch) {
		return indexOf(ch, 0);
	}
	
	
	protected int indexOf(long ch) {
		return indexOf(ch, 0);
	}
	
	
	protected int indexOf(double ch) {
		return indexOf(ch, 0);
	}
	
	
	protected int indexOf(float ch) {
		return indexOf(ch, 0);
	}
	
	
	protected int indexOf(Object ch) {
		return indexOf(ch, 0);
	}
	
	
	protected int indexOf(byte ch, int fromIndex) {
		for(int i = fromIndex; i < length(); i++) {
			if(equalsAt(i, ch)) {
				return i;
			}
		}
		return -1;
	}
	
	
	protected int indexOf(char ch, int fromIndex) {
		for(int i = fromIndex; i < length(); i++) {
			if(equalsAt(i, ch)) {
				return i;
			}
		}
		return -1;
	}
	
	
	protected int indexOf(short ch, int fromIndex) {
		for(int i = fromIndex; i < length(); i++) {
			if(equalsAt(i, ch)) {
				return i;
			}
		}
		return -1;
	}
	
	
	protected int indexOf(int ch, int fromIndex) {
		for(int i = fromIndex; i < length(); i++) {
			if(equalsAt(i, ch)) {
				return i;
			}
		}
		return -1;
	}
	
	
	protected int indexOf(long ch, int fromIndex) {
		for(int i = fromIndex; i < length(); i++) {
			if(equalsAt(i, ch)) {
				return i;
			}
		}
		return -1;
	}
	
	
	protected int indexOf(double ch, int fromIndex) {
		for(int i = fromIndex; i < length(); i++) {
			if(equalsAt(i, ch)) {
				return i;
			}
		}
		return -1;
	}
	
	
	protected int indexOf(float ch, int fromIndex) {
		for(int i = fromIndex; i < length(); i++) {
			if(equalsAt(i, ch)) {
				return i;
			}
		}
		return -1;
	}
	
	
	protected int indexOf(Object ch, int fromIndex) {
		for(int i = fromIndex; i < length(); i++) {
			if(equalsAt(i, ch)) {
				return i;
			}
		}
		return -1;
	}
	
	
	public int indexOf(S s) {
		return indexOf(s, 0);
	}
	
	
	public int indexOf(S s, int fromIndex) {
		loop1:
		for(int i = fromIndex; i <= length() - s.length(); i++) {
			if(equalsAt(i, s, i - fromIndex)) {
				for(int j = 1; j < s.length(); j++) {
					if(!equalsAt(j + i, s, j)) {
						continue loop1;
					}
				}
				return i;
			}
		}
		return -1;
	}
	
	
	protected int lastIndexOf(byte ch) {
		return lastIndexOf(ch, 0);
	}
	
	
	protected int lastIndexOf(char ch) {
		return lastIndexOf(ch, 0);
	}
	
	
	protected int lastIndexOf(short ch) {
		return lastIndexOf(ch, 0);
	}
	
	
	protected int lastIndexOf(int ch) {
		return lastIndexOf(ch, 0);
	}
	
	
	protected int lastIndexOf(long ch) {
		return lastIndexOf(ch, 0);
	}
	
	
	protected int lastIndexOf(double ch) {
		return lastIndexOf(ch, 0);
	}
	
	
	protected int lastIndexOf(float ch) {
		return lastIndexOf(ch, 0);
	}
	
	
	protected int lastIndexOf(Object ch) {
		return lastIndexOf(ch, 0);
	}
	
	
	protected int lastIndexOf(byte ch, int fromIndex) {
		for(int i = length() - 1; i >= fromIndex; i--) {
			if(equalsAt(i, ch)) {
				return i;
			}
		}
		return -1;
	}
	
	
	protected int lastIndexOf(char ch, int fromIndex) {
		for(int i = length() - 1; i >= fromIndex; i--) {
			if(equalsAt(i, ch)) {
				return i;
			}
		}
		return -1;
	}
	
	
	protected int lastIndexOf(short ch, int fromIndex) {
		for(int i = length() - 1; i >= fromIndex; i--) {
			if(equalsAt(i, ch)) {
				return i;
			}
		}
		return -1;
	}
	
	
	protected int lastIndexOf(int ch, int fromIndex) {
		for(int i = length() - 1; i >= fromIndex; i--) {
			if(equalsAt(i, ch)) {
				return i;
			}
		}
		return -1;
	}
	
	
	protected int lastIndexOf(long ch, int fromIndex) {
		for(int i = length() - 1; i >= fromIndex; i--) {
			if(equalsAt(i, ch)) {
				return i;
			}
		}
		return -1;
	}
	
	
	protected int lastIndexOf(double ch, int fromIndex) {
		for(int i = length() - 1; i >= fromIndex; i--) {
			if(equalsAt(i, ch)) {
				return i;
			}
		}
		return -1;
	}
	
	
	protected int lastIndexOf(float ch, int fromIndex) {
		for(int i = length() - 1; i >= fromIndex; i--) {
			if(equalsAt(i, ch)) {
				return i;
			}
		}
		return -1;
	}
	
	
	protected int lastIndexOf(Object ch, int fromIndex) {
		for(int i = length() - 1; i >= fromIndex; i--) {
			if(equalsAt(i, ch)) {
				return i;
			}
		}
		return -1;
	}
	
	
	public int lastIndexOf(S s) {
		return lastIndexOf(s, 0);
	}
	
	
	public int lastIndexOf(S s, int fromIndex) {
		loop1:
		for(int i = length() - s.length(); i >= fromIndex; i--) {
			if(equalsAt(i, s, i - fromIndex)) {
				for(int j = 1; j < s.length(); j++) {
					if(!equalsAt(j + i, s, j)) {
						continue loop1;
					}
				}
				return i;
			}
		}
		return -1;
	}
	
	
	public boolean regionMatches(
			int toffset,
			S s,
            int ooffset,
            int len) {
		if(toffset < 0) {
			return false;
		} else if(ooffset < 0) {
			return false;
		} else if(toffset + len > length()) {
			return false;
		} else if(ooffset + len > s.length()) {
			return false;
		}
		
		for(int i = 0; i < len; i++) {
			if(!equalsAt(i + toffset, s, i + ooffset)) {
				return false;
			}
		}
		return true;
	}
	
	
	protected S replace(byte oldChar, byte newChar) {
		byte[] res = copyByteArray();
		
		for(int i = 0; i < res.length; i++) {
			if(res[i] == oldChar) {
				res[i] = newChar;
			}
		}
		return returnString(res);
	}
	
	
	protected S replace(char oldChar, char newChar) {
		char[] res = copyCharArray();
		
		for(int i = 0; i < res.length; i++) {
			if(res[i] == oldChar) {
				res[i] = newChar;
			}
		}
		return returnString(res);
	}
	
	
	protected S replace(short oldChar, short newChar) {
		short[] res = copyShortArray();
		
		for(int i = 0; i < res.length; i++) {
			if(res[i] == oldChar) {
				res[i] = newChar;
			}
		}
		return returnString(res);
	}
	
	
	protected S replace(int oldChar, int newChar) {
		int[] res = copyIntArray();
		
		for(int i = 0; i < res.length; i++) {
			if(res[i] == oldChar) {
				res[i] = newChar;
			}
		}
		return returnString(res);
	}
	
	
	protected S replace(long oldChar, long newChar) {
		long[] res = copyLongArray();
		
		for(int i = 0; i < res.length; i++) {
			if(res[i] == oldChar) {
				res[i] = newChar;
			}
		}
		return returnString(res);
	}
	
	
	protected S replace(double oldChar, double newChar) {
		double[] res = copyDoubleArray();
		
		for(int i = 0; i < res.length; i++) {
			if(res[i] == oldChar) {
				res[i] = newChar;
			}
		}
		return returnString(res);
	}
	
	
	protected S replace(float oldChar, float newChar) {
		float[] res = copyFloatArray();
		
		for(int i = 0; i < res.length; i++) {
			if(res[i] == oldChar) {
				res[i] = newChar;
			}
		}
		return returnString(res);
	}
	
	
	protected S replace(Object oldChar, Object newChar) {
		Object[] res = copyObjectArray();
		
		for(int i = 0; i < res.length; i++) {
			if(Objects.equals(oldChar, newChar)) {
				res[i] = newChar;
			}
		}
		return returnString(res);
	}
	
	
	public boolean startsWith(S b) {
		if(b.length() > length()) {
			return false;
		}
		
		for(int i = 0; i < b.length(); i++) {
			if(!equalsAt(i, b, i)) {
				return false;
			}
		}
		return true;
	}
	
	
	public S substring(int beginIndex) {
		return substring(beginIndex, length());
	}
	
	
	public S substring(int beginIndex, int endIndex) {
		Object res;
		
		if(beginIndex < 0) {
			throw new IllegalArgumentException();
		} else if(endIndex < 0) {
			throw new IllegalArgumentException();
		} else if(endIndex > length()) {
			throw new IllegalArgumentException();
		}
		
		res = newArray(endIndex - beginIndex);
		System.arraycopy(
				getArray(), beginIndex,
				res, 0, endIndex - beginIndex);
		return returnString(res);
	}
	
}
