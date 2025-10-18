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
public class AsDouble {
	
	/**
	 * 
	 * @param a
	 * @return
	 */
	public static DoubleVector valuesOf(final double... a) {
		if(a == null) {
			throw new NullPointerException();
		}
		
		return new AbstractDoubleVector() {

			public int size() {
				java.util.Arrays.asList();
				return a.length;
			}

			public void addDouble(int index, double v) {
				throw new UnsupportedOperationException();
			}

			public double getDouble(int index) {
				if(index < 0 || index >= a.length) {
					throw new IndexOutOfBoundsException();
				}
				return a[index];
			}

			public double removeAt(int index) {
				throw new UnsupportedOperationException();
			}

			public double setDouble(int index, double v) {
				double r;
				
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

			public double[] toDoubleArray() {
				return (double[])a.clone();
			}

			public double[] toDoubleArray(double[] ar) {
				if(a.length > ar.length) {
					return toDoubleArray();
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
	public static DoubleVector vector(final int... a) {
		if(a == null) {
			throw new NullPointerException();
		}
		
		//
		final double[] a2 = new double[a.length];
		for(int i = 0; i < a.length; i++) {
			a2[i] = (double)a[i];
		}
		
		return new AbstractDoubleVector() {

			public int size() {
				return a2.length;
			}

			public void addDouble(int index, double v) {
				throw new UnsupportedOperationException();
			}

			public double getDouble(int index) {
				if(index < 0 || index >= a2.length) {
					throw new IndexOutOfBoundsException();
				}
				return a2[index];
			}

			public double removeAt(int index) {
				throw new UnsupportedOperationException();
			}

			public double setDouble(int index, double v) {
				double r;
				
				if(index < 0 || index >= a2.length) {
					throw new IndexOutOfBoundsException();
				}
				r = (double)a2[index];
				a2[index] = v;
				return r;
			}

			public void clear() {
				throw new IndexOutOfBoundsException();
			}
			
		};
	}
	
}
