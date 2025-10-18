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
package net.morilib.util.primitive;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/11/03
 */
public class AsCharacter {
	
	/**
	 * 
	 * @param a
	 * @return
	 */
	public static CharacterVector valuesOf(final char... a) {
		if(a == null) {
			throw new NullPointerException();
		}
		
		return new AbstractCharacterVector() {

			public int size() {
				java.util.Arrays.asList();
				return a.length;
			}

			public void addChar(int index, char v) {
				throw new UnsupportedOperationException();
			}

			public char getChar(int index) {
				if(index < 0 || index >= a.length) {
					throw new IndexOutOfBoundsException();
				}
				return a[index];
			}

			public char removeAt(int index) {
				throw new UnsupportedOperationException();
			}

			public char setChar(int index, char v) {
				char r;
				
				if(index < 0 || index >= a.length) {
					throw new IndexOutOfBoundsException();
				}
				r = a[index];
				a[index] = v;
				return r;
			}

			public void clear() {
				throw new IndexOutOfBoundsException();
			}

			public char[] toCharArray() {
				return (char[])a.clone();
			}

			public char[] toCharArray(char[] ar) {
				if(a.length > ar.length) {
					return toCharArray();
				} else {
					System.arraycopy(ar, 0, a, 0, a.length);
					return ar;
				}
			}
			
		};
	}
	
	/**
	 * 
	 * @param a
	 * @return
	 */
	public static CharacterVector vector(final int... a) {
		if(a == null) {
			throw new NullPointerException();
		} else {
			for(int i = 0; i < a.length; i++) {
				if(a[i] < Character.MIN_VALUE ||
						a[i] > Character.MAX_VALUE) {
					throw new IllegalArgumentException();
				}
			}
		}
		
		return new AbstractCharacterVector() {

			public int size() {
				java.util.Arrays.asList();
				return a.length;
			}

			public void addChar(int index, char v) {
				throw new UnsupportedOperationException();
			}

			public char getChar(int index) {
				if(index < 0 || index >= a.length) {
					throw new IndexOutOfBoundsException();
				}
				return (char)a[index];
			}

			public char removeAt(int index) {
				throw new UnsupportedOperationException();
			}

			public char setChar(int index, char v) {
				char r;
				
				if(index < 0 || index >= a.length) {
					throw new IndexOutOfBoundsException();
				}
				r = (char)a[index];
				a[index] = v;
				return r;
			}

			public void clear() {
				throw new IndexOutOfBoundsException();
			}
			
		};
	}
	
}
