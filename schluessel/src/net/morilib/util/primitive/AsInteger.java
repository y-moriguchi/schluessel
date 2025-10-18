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
public class AsInteger {
	
	/**
	 * 
	 * @param a
	 * @return
	 */
	public static IntegerVector valuesOf(final int... a) {
		if(a == null) {
			throw new NullPointerException();
		}
		
		return new AbstractIntegerVector() {

			public int size() {
				java.util.Arrays.asList();
				return a.length;
			}

			public void addInt(int index, int v) {
				throw new UnsupportedOperationException();
			}

			public int getInt(int index) {
				if(index < 0 || index >= a.length) {
					throw new IndexOutOfBoundsException();
				}
				return a[index];
			}

			public int removeAt(int index) {
				throw new UnsupportedOperationException();
			}

			public int setInt(int index, int v) {
				int r;
				
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

			public int[] toIntArray() {
				return (int[])a.clone();
			}

			public int[] toIntArray(int[] ar) {
				if(a.length > ar.length) {
					return toIntArray();
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
	public static IntegerVector vector(final int... a) {
		if(a == null) {
			throw new NullPointerException();
		}
		
		return new AbstractIntegerVector() {

			public int size() {
				java.util.Arrays.asList();
				return a.length;
			}

			public void addInt(int index, int v) {
				throw new UnsupportedOperationException();
			}

			public int getInt(int index) {
				if(index < 0 || index >= a.length) {
					throw new IndexOutOfBoundsException();
				}
				return (int)a[index];
			}

			public int removeAt(int index) {
				throw new UnsupportedOperationException();
			}

			public int setInt(int index, int v) {
				int r;
				
				if(index < 0 || index >= a.length) {
					throw new IndexOutOfBoundsException();
				}
				r = (int)a[index];
				a[index] = v;
				return r;
			}

			public void clear() {
				throw new IndexOutOfBoundsException();
			}
			
		};
	}
	
}
