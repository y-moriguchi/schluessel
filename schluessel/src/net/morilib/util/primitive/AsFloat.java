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
public class AsFloat {
	
	/**
	 * 
	 * @param a
	 * @return
	 */
	public static FloatVector valuesOf(final float... a) {
		if(a == null) {
			throw new NullPointerException();
		}
		
		return new AbstractFloatVector() {

			public int size() {
				java.util.Arrays.asList();
				return a.length;
			}

			public void addFloat(int index, float v) {
				throw new UnsupportedOperationException();
			}

			public float getFloat(int index) {
				if(index < 0 || index >= a.length) {
					throw new IndexOutOfBoundsException();
				}
				return a[index];
			}

			public float removeAt(int index) {
				throw new UnsupportedOperationException();
			}

			public float setFloat(int index, float v) {
				float r;
				
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

			public float[] toFloatArray() {
				return (float[])a.clone();
			}

			public float[] toFloatArray(float[] ar) {
				if(a.length > ar.length) {
					return toFloatArray();
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
	public static FloatVector vector(int... a) {
		if(a == null) {
			throw new NullPointerException();
		}
		
		//
		final float[] a2 = new float[a.length];
		for(int i = 0; i < a.length; i++) {
			a2[i] = (float)a[i];
		}
		
		return new AbstractFloatVector() {

			public int size() {
				return a2.length;
			}

			public void addFloat(int index, float v) {
				throw new UnsupportedOperationException();
			}

			public float getFloat(int index) {
				if(index < 0 || index >= a2.length) {
					throw new IndexOutOfBoundsException();
				}
				return a2[index];
			}

			public float removeAt(int index) {
				throw new UnsupportedOperationException();
			}

			public float setFloat(int index, float v) {
				float r;
				
				if(index < 0 || index >= a2.length) {
					throw new IndexOutOfBoundsException();
				}
				r = a2[index];
				a2[index] = v;
				return r;
			}

			public void clear() {
				throw new IndexOutOfBoundsException();
			}
			
		};
	}
	
}
