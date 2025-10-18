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
public class AsLong {
	
	/**
	 * 
	 * @param a
	 * @return
	 */
	public static LongVector valuesOf(final long... a) {
		if(a == null) {
			throw new NullPointerException();
		}
		
		return new AbstractLongVector() {

			public int size() {
				java.util.Arrays.asList();
				return a.length;
			}

			public void addLong(int index, long v) {
				throw new UnsupportedOperationException();
			}

			public long getLong(int index) {
				if(index < 0 || index >= a.length) {
					throw new IndexOutOfBoundsException();
				}
				return a[index];
			}

			public long removeAt(int index) {
				throw new UnsupportedOperationException();
			}

			public long setLong(int index, long v) {
				long r;
				
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

			public long[] toLongArray() {
				return (long[])a.clone();
			}

			public long[] toLongArray(long[] ar) {
				if(a.length > ar.length) {
					return toLongArray();
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
	public static LongVector vector(final int... a) {
		if(a == null) {
			throw new NullPointerException();
		}
		
		//
		final long[] a2 = new long[a.length];
		for(int i = 0; i < a.length; i++) {
			a2[i] = (long)a[i];
		}
		
		return new AbstractLongVector() {

			public int size() {
				return a2.length;
			}

			public void addLong(int index, long v) {
				throw new UnsupportedOperationException();
			}

			public long getLong(int index) {
				if(index < 0 || index >= a2.length) {
					throw new IndexOutOfBoundsException();
				}
				return (long)a2[index];
			}

			public long removeAt(int index) {
				throw new UnsupportedOperationException();
			}

			public long setLong(int index, long v) {
				long r;
				
				if(index < 0 || index >= a2.length) {
					throw new IndexOutOfBoundsException();
				}
				r = (long)a2[index];
				a2[index] = v;
				return r;
			}

			public void clear() {
				throw new IndexOutOfBoundsException();
			}
			
		};
	}
	
}
