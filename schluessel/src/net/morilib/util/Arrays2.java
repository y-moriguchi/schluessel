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
package net.morilib.util;

/**
 * <i>USEful Implements</i> for arrays.<br>
 * 配列に関する便利な関数である.
 * 
 * @author MORIGUCHI, Yuichiro 2004/12/18
 */
public final class Arrays2 {
	
	//
	private static final String DELIM = ", ";
	
	/**
	 * An empty array of int.
	 * <p>空のint型配列である.
	 */
	public static final int[] INT_EMPTY = new int[0];
	
	/**
	 * An empty array of byte.
	 * <p>空のbyte型配列である.
	 */
	public static final byte[] BYTE_EMPTY = new byte[0];
	
	/**
	 * An empty array of short.
	 * <p>空のshort型配列である.
	 */
	public static final short[] SHORT_EMPTY = new short[0];
	
	/**
	 * An empty array of long.
	 * <p>空のlong型配列である.
	 */
	public static final long[] LONG_EMPTY = new long[0];
	
	/**
	 * An empty array of char.
	 * <p>空のchar型配列である.
	 */
	public static final char[] CHAR_EMPTY = new char[0];
	
	/**
	 * An empty array of float.
	 * <p>空のfloat型配列である.
	 */
	public static final float[] FLOAT_EMPTY = new float[0];
	
	/**
	 * An empty array of double.
	 * <p>空のdouble型配列である.
	 */
	public static final double[] DOUBLE_EMPTY = new double[0];
	
	/**
	 * An empty array of String.
	 * <p>空のString配列である.
	 */
	public static final String[] STRING_EMPTY = new String[0];
	
	/**
	 * An empty array of Object.
	 * <p>空のObject型配列である.
	 */
	public static final Object[] OBJECT_EMPTY = new Object[0];
	
	//
	private Arrays2() {}
	
	/**
	 * returns true if the given arrays equals.
	 * <p>引数の配列が等しいときにtrueを得る.
	 * 
	 * @param a1  the first array to be compared
	 * @param a2  the second array to be compared
	 * @return  true if the given arrays equals
	 */
	public static boolean equals(Object[] a1, Object[] a2) {
		if(a1.length != a2.length) {
			return false;
		} else {
			for(int i = 0; i < a1.length; i++) {
				if(!Objects.equals(a1[i], a2[i])) {
					return false;
				}
			}
			return true;
		}
	}
	
	/**
	 * returns true if the given arrays equals.
	 * <p>引数の配列が等しいときにtrueを得る.
	 * 
	 * @param a1  the first array to be compared
	 * @param a2  the second array to be compared
	 * @return  true if the given arrays equals
	 */
	public static boolean equals(int[] a1, int[] a2) {
		if(a1.length != a2.length) {
			return false;
		} else {
			for(int i = 0; i < a1.length; i++) {
				if(a1[i] != a2[i]) {
					return false;
				}
			}
			return true;
		}
	}
	
	/**
	 * returns true if the given arrays equals.
	 * <p>引数の配列が等しいときにtrueを得る.
	 * 
	 * @param a1  the first array to be compared
	 * @param a2  the second array to be compared
	 * @return  true if the given arrays equals
	 */
	public static boolean equals(long[] a1, long[] a2) {
		if(a1.length != a2.length) {
			return false;
		} else {
			for(int i = 0; i < a1.length; i++) {
				if(a1[i] != a2[i]) {
					return false;
				}
			}
			return true;
		}
	}
	
	/**
	 * returns true if the given arrays equals.
	 * <p>引数の配列が等しいときにtrueを得る.
	 * 
	 * @param a1  the first array to be compared
	 * @param a2  the second array to be compared
	 * @return  true if the given arrays equals
	 */
	public static boolean equals(short[] a1, short[] a2) {
		if(a1.length != a2.length) {
			return false;
		} else {
			for(int i = 0; i < a1.length; i++) {
				if(a1[i] != a2[i]) {
					return false;
				}
			}
			return true;
		}
	}
	
	/**
	 * returns true if the given arrays equals.
	 * <p>引数の配列が等しいときにtrueを得る.
	 * 
	 * @param a1  the first array to be compared
	 * @param a2  the second array to be compared
	 * @return  true if the given arrays equals
	 */
	public static boolean equals(byte[] a1, byte[] a2) {
		if(a1.length != a2.length) {
			return false;
		} else {
			for(int i = 0; i < a1.length; i++) {
				if(a1[i] != a2[i]) {
					return false;
				}
			}
			return true;
		}
	}
	
	/**
	 * returns true if the given arrays equals.
	 * <p>引数の配列が等しいときにtrueを得る.
	 * 
	 * @param a1  the first array to be compared
	 * @param a2  the second array to be compared
	 * @return  true if the given arrays equals
	 */
	public static boolean equals(char[] a1, char[] a2) {
		if(a1.length != a2.length) {
			return false;
		} else {
			for(int i = 0; i < a1.length; i++) {
				if(a1[i] != a2[i]) {
					return false;
				}
			}
			return true;
		}
	}
	
	/**
	 * returns true if the given arrays equals.
	 * <p>引数の配列が等しいときにtrueを得る.
	 * 
	 * @param a1  the first array to be compared
	 * @param a2  the second array to be compared
	 * @return  true if the given arrays equals
	 */
	public static boolean equals(float[] a1, float[] a2) {
		if(a1.length != a2.length) {
			return false;
		} else {
			for(int i = 0; i < a1.length; i++) {
				if(a1[i] != a2[i]) {
					return false;
				}
			}
			return true;
		}
	}
	
	/**
	 * returns true if the given arrays equals.
	 * <p>引数の配列が等しいときにtrueを得る.
	 * 
	 * @param a1  the first array to be compared
	 * @param a2  the second array to be compared
	 * @return  true if the given arrays equals
	 */
	public static boolean equals(double[] a1, double[] a2) {
		if(a1.length != a2.length) {
			return false;
		} else {
			for(int i = 0; i < a1.length; i++) {
				if(a1[i] != a2[i]) {
					return false;
				}
			}
			return true;
		}
	}
	
	/**
	 * returns true if the given arrays equals.
	 * <p>引数の配列が等しいときにtrueを得る.
	 * 
	 * @param a1  the first array to be compared
	 * @param a2  the second array to be compared
	 * @return  true if the given arrays equals
	 */
	public static boolean equals(boolean[] a1, boolean[] a2) {
		if(a1.length != a2.length) {
			return false;
		} else {
			for(int i = 0; i < a1.length; i++) {
				if(a1[i] != a2[i]) {
					return false;
				}
			}
			return true;
		}
	}
	
	/**
	 * copies the given source array to the given destination array.
	 * <p>配列をコピーする.
	 * 
	 * @param src   the source array
	 * @param begin the starting index in the source array
	 * @param end   the ending index in the source array
	 * @param dst   the destination array
	 * @return  the given destination array
	 */
	public static<E> E[] copy(E[] src, int begin, int end, E[] dst) {
		System.arraycopy(src, begin, dst, 0, end - begin + 1);
		return dst;
	}
	
	/**
	 * copies the given source array to the given destination array.
	 * <p>配列をコピーする.
	 * 
	 * @param src   the source array
	 * @param begin the starting index in the source array
	 * @param end   the ending index in the source array
	 * @return  a clone of the source array
	 */
	@SuppressWarnings("unchecked")
	public static<E> E[] copy(E[] src, int begin, int end) {
		return (E[])copy(src, begin, end, new Object[end - begin + 1]);
	}
	
	/**
	 * copies the given source array to the given destination array.
	 * <p>配列をコピーする.
	 * 
	 * @param src   the source array
	 * @param dst   the destination array
	 * @return  the given destination array
	 */
	public static<E> E[] copy(E[] src, E[] dst) {
		return copy(src, 0, src.length - 1, dst);
	}
	
	/**
	 * copies the given source array to the given destination array.
	 * <p>配列をコピーする.
	 * 
	 * @param src   the source array
	 * @return  a clone of the source array
	 */
	@SuppressWarnings("unchecked")
	public static<E> E[] copy(E[] src) {
		return (E[])copy(src, new Object[src.length]);
	}
	
	/**
	 * copies the given source array to the given destination array.
	 * <p>配列をコピーする.
	 * 
	 * @param src   the source array
	 * @param begin the starting index in the source array
	 * @param end   the ending index in the source array
	 * @param dst   the destination array
	 * @return  the given destination array
	 */
	public static boolean[] copy(
			boolean[] src, int begin, int end, boolean[] dst) {
		System.arraycopy(src, begin, dst, 0, end - begin + 1);
		return dst;
	}
	
	/**
	 * copies the given source array to the given destination array.
	 * <p>配列をコピーする.
	 * 
	 * @param src   the source array
	 * @param begin the starting index in the source array
	 * @param end   the ending index in the source array
	 * @return  a clone of the source array
	 */
	public static boolean[] copy(boolean[] src, int begin, int end) {
		return copy(src, begin, end, new boolean[end - begin + 1]);
	}
	
	/**
	 * copies the given source array to the given destination array.
	 * <p>配列をコピーする.
	 * 
	 * @param src   the source array
	 * @param dst   the destination array
	 * @return  the given destination array
	 */
	public static boolean[] copy(boolean[] src, boolean[] dst) {
		return copy(src, 0, src.length - 1, dst);
	}
	
	/**
	 * copies the given source array to the given destination array.
	 * <p>配列をコピーする.
	 * 
	 * @param src   the source array
	 * @return  a clone of the source array
	 */
	public static boolean[] copy(boolean[] src) {
		return copy(src, 0, src.length - 1, new boolean[src.length]);
	}
	
	/**
	 * copies the given source array to the given destination array.
	 * <p>配列をコピーする.
	 * 
	 * @param src   the source array
	 * @param begin the starting index in the source array
	 * @param end   the ending index in the source array
	 * @param dst   the destination array
	 * @return  the given destination array
	 */
	public static byte[] copy(
			byte[] src, int begin, int end, byte[] dst) {
		System.arraycopy(src, begin, dst, 0, end - begin + 1);
		return dst;
	}
	
	/**
	 * copies the given source array to the given destination array.
	 * <p>配列をコピーする.
	 * 
	 * @param src   the source array
	 * @param begin the starting index in the source array
	 * @param end   the ending index in the source array
	 * @return  a clone of the source array
	 */
	public static byte[] copy(byte[] src, int begin, int end) {
		return copy(src, begin, end, new byte[end - begin + 1]);
	}
	
	/**
	 * copies the given source array to the given destination array.
	 * <p>配列をコピーする.
	 * 
	 * @param src   the source array
	 * @param dst   the destination array
	 * @return  the given destination array
	 */
	public static byte[] copy(byte[] src, byte[] dst) {
		return copy(src, 0, src.length - 1, dst);
	}
	
	/**
	 * copies the given source array to the given destination array.
	 * <p>配列をコピーする.
	 * 
	 * @param src   the source array
	 * @return  a clone of the source array
	 */
	public static byte[] copy(byte[] src) {
		return copy(src, 0, src.length - 1, new byte[src.length]);
	}
	
	/**
	 * copies the given source array to the given destination array.
	 * <p>配列をコピーする.
	 * 
	 * @param src   the source array
	 * @param begin the starting index in the source array
	 * @param end   the ending index in the source array
	 * @param dst   the destination array
	 * @return  the given destination array
	 */
	public static char[] copy(
			char[] src, int begin, int end, char[] dst) {
		System.arraycopy(src, begin, dst, 0, end - begin + 1);
		return dst;
	}
	
	/**
	 * copies the given source array to the given destination array.
	 * <p>配列をコピーする.
	 * 
	 * @param src   the source array
	 * @param begin the starting index in the source array
	 * @param end   the ending index in the source array
	 * @return  a clone of the source array
	 */
	public static char[] copy(char[] src, int begin, int end) {
		return copy(src, begin, end, new char[end - begin + 1]);
	}
	
	/**
	 * copies the given source array to the given destination array.
	 * <p>配列をコピーする.
	 * 
	 * @param src   the source array
	 * @param dst   the destination array
	 * @return  the given destination array
	 */
	public static char[] copy(char[] src, char[] dst) {
		return copy(src, 0, src.length - 1, dst);
	}
	
	/**
	 * copies the given source array to the given destination array.
	 * <p>配列をコピーする.
	 * 
	 * @param src   the source array
	 * @return  a clone of the source array
	 */
	public static char[] copy(char[] src) {
		return copy(src, 0, src.length - 1, new char[src.length]);
	}
	
	/**
	 * copies the given source array to the given destination array.
	 * <p>配列をコピーする.
	 * 
	 * @param src   the source array
	 * @param begin the starting index in the source array
	 * @param end   the ending index in the source array
	 * @param dst   the destination array
	 * @return  the given destination array
	 */
	public static short[] copy(
			short[] src, int begin, int end, short[] dst) {
		System.arraycopy(src, begin, dst, 0, end - begin + 1);
		return dst;
	}
	
	/**
	 * copies the given source array to the given destination array.
	 * <p>配列をコピーする.
	 * 
	 * @param src   the source array
	 * @param begin the starting index in the source array
	 * @param end   the ending index in the source array
	 * @return  a clone of the source array
	 */
	public static short[] copy(short[] src, int begin, int end) {
		return copy(src, begin, end, new short[end - begin + 1]);
	}
	
	/**
	 * copies the given source array to the given destination array.
	 * <p>配列をコピーする.
	 * 
	 * @param src   the source array
	 * @param dst   the destination array
	 * @return  the given destination array
	 */
	public static short[] copy(short[] src, short[] dst) {
		return copy(src, 0, src.length - 1, dst);
	}
	
	/**
	 * copies the given source array to the given destination array.
	 * <p>配列をコピーする.
	 * 
	 * @param src   the source array
	 * @return  a clone of the source array
	 */
	public static short[] copy(short[] src) {
		return copy(src, 0, src.length - 1, new short[src.length]);
	}
	
	/**
	 * copies the given source array to the given destination array.
	 * <p>配列をコピーする.
	 * 
	 * @param src   the source array
	 * @param begin the starting index in the source array
	 * @param end   the ending index in the source array
	 * @param dst   the destination array
	 * @return  the given destination array
	 */
	public static int[] copy(int[] src, int begin, int end, int[] dst) {
		System.arraycopy(src, begin, dst, 0, end - begin + 1);
		return dst;
	}
	
	/**
	 * copies the given source array to the given destination array.
	 * <p>配列をコピーする.
	 * 
	 * @param src   the source array
	 * @param begin the starting index in the source array
	 * @param end   the ending index in the source array
	 * @return  a clone of the source array
	 */
	public static int[] copy(int[] src, int begin, int end) {
		return copy(src, begin, end, new int[end - begin + 1]);
	}
	
	/**
	 * copies the given source array to the given destination array.
	 * <p>配列をコピーする.
	 * 
	 * @param src   the source array
	 * @param dst   the destination array
	 * @return  the given destination array
	 */
	public static int[] copy(int[] src, int[] dst) {
		return copy(src, 0, src.length - 1, dst);
	}
	
	/**
	 * copies the given source array to the given destination array.
	 * <p>配列をコピーする.
	 * 
	 * @param src   the source array
	 * @return  a clone of the source array
	 */
	public static int[] copy(int[] src) {
		return copy(src, 0, src.length - 1, new int[src.length]);
	}
	
	/**
	 * copies the given source array to the given destination array.
	 * <p>配列をコピーする.
	 * 
	 * @param src   the source array
	 * @param begin the starting index in the source array
	 * @param end   the ending index in the source array
	 * @param dst   the destination array
	 * @return  the given destination array
	 */
	public static long[] copy(
			long[] src, int begin, int end, long[] dst) {
		System.arraycopy(src, begin, dst, 0, end - begin + 1);
		return dst;
	}
	
	/**
	 * copies the given source array to the given destination array.
	 * <p>配列をコピーする.
	 * 
	 * @param src   the source array
	 * @param begin the starting index in the source array
	 * @param end   the ending index in the source array
	 * @return  a clone of the source array
	 */
	public static long[] copy(long[] src, int begin, int end) {
		return copy(src, begin, end, new long[end - begin + 1]);
	}
	
	/**
	 * copies the given source array to the given destination array.
	 * <p>配列をコピーする.
	 * 
	 * @param src   the source array
	 * @param dst   the destination array
	 * @return  the given destination array
	 */
	public static long[] copy(long[] src, long[] dst) {
		return copy(src, 0, src.length - 1, dst);
	}
	
	/**
	 * copies the given source array to the given destination array.
	 * <p>配列をコピーする.
	 * 
	 * @param src   the source array
	 * @return  a clone of the source array
	 */
	public static long[] copy(long[] src) {
		return copy(src, 0, src.length - 1, new long[src.length]);
	}
	
	/**
	 * copies the given source array to the given destination array.
	 * <p>配列をコピーする.
	 * 
	 * @param src   the source array
	 * @param begin the starting index in the source array
	 * @param end   the ending index in the source array
	 * @param dst   the destination array
	 * @return  the given destination array
	 */
	public static float[] copy(
			float[] src, int begin, int end, float[] dst) {
		System.arraycopy(src, begin, dst, 0, end - begin + 1);
		return dst;
	}
	
	/**
	 * copies the given source array to the given destination array.
	 * <p>配列をコピーする.
	 * 
	 * @param src   the source array
	 * @param begin the starting index in the source array
	 * @param end   the ending index in the source array
	 * @return  a clone of the source array
	 */
	public static float[] copy(float[] src, int begin, int end) {
		return copy(src, begin, end, new float[end - begin + 1]);
	}
	
	/**
	 * copies the given source array to the given destination array.
	 * <p>配列をコピーする.
	 * 
	 * @param src   the source array
	 * @param dst   the destination array
	 * @return  the given destination array
	 */
	public static float[] copy(float[] src, float[] dst) {
		return copy(src, 0, src.length - 1, dst);
	}
	
	/**
	 * copies the given source array to the given destination array.
	 * <p>配列をコピーする.
	 * 
	 * @param src   the source array
	 * @return  a clone of the source array
	 */
	public static float[] copy(float[] src) {
		return copy(src, 0, src.length - 1, new float[src.length]);
	}
	
	/**
	 * copies the given source array to the given destination array.
	 * <p>配列をコピーする.
	 * 
	 * @param src   the source array
	 * @param begin the starting index in the source array
	 * @param end   the ending index in the source array
	 * @param dst   the destination array
	 * @return  the given destination array
	 */
	public static double[] copy(
			double[] src, int begin, int end, double[] dst) {
		System.arraycopy(src, begin, dst, 0, end - begin + 1);
		return dst;
	}
	
	/**
	 * copies the given source array to the given destination array.
	 * <p>配列をコピーする.
	 * 
	 * @param src   the source array
	 * @param begin the starting index in the source array
	 * @param end   the ending index in the source array
	 * @return  a clone of the source array
	 */
	public static double[] copy(double[] src, int begin, int end) {
		return copy(src, begin, end, new double[end - begin + 1]);
	}
	
	/**
	 * copies the given source array to the given destination array.
	 * <p>配列をコピーする.
	 * 
	 * @param src   the source array
	 * @param dst   the destination array
	 * @return  the given destination array
	 */
	public static double[] copy(double[] src, double[] dst) {
		return copy(src, 0, src.length - 1, dst);
	}
	
	/**
	 * copies the given source array to the given destination array.
	 * <p>配列をコピーする.
	 * 
	 * @param src   the source array
	 * @return  a clone of the source array
	 */
	public static double[] copy(double[] src) {
		return copy(src, 0, src.length - 1, new double[src.length]);
	}
	
	/**
	 * returns true if the all elements of the given array is null.
	 * <p>配列の全要素がnullのときtrueを得る.
	 * 
	 * @param arr  the array to be tested
	 * @return  if the all elements of the given array is null
	 */
	public static boolean isAllNull(Object[] arr) {
		for(int i = 0; i < arr.length; i++) {
			if(arr[i] != null) {
				return false;
			}
		}
		return true;
	}
	
	/**
	 * returns true if the all elements of the given array is false.
	 * <p>配列の全要素がfalseのときtrueを得る.
	 * 
	 * @param arr  the array to be tested
	 * @return  if the all elements of the given array is false
	 */
	public static boolean isAllFalse(boolean[] arr) {
		for(int i = 0; i < arr.length; i++) {
			if(arr[i]) {
				return false;
			}
		}
		return true;
	}
	
	/**
	 * returns true if the all elements of the given array is true.
	 * <p>配列の全要素がtrueのときtrueを得る.
	 * 
	 * @param arr  the array to be tested
	 * @return  if the all elements of the given array is true
	 */
	public static boolean isAllTrue(boolean[] arr) {
		for(int i = 0; i < arr.length; i++) {
			if(!arr[i]) {
				return false;
			}
		}
		return true;
	}
	
	/**
	 * returns true if the all elements of the given array is zero.
	 * <p>配列の全要素が0のときtrueを得る.
	 * 
	 * @param arr  the array to be tested
	 * @return  if the all elements of the given array is null
	 */
	public static boolean isEmpty(byte[] arr) {
		for(int i = 0; i < arr.length; i++) {
			if(arr[i] != 0)
				return false;
		}
		return true;
	}
	
	/**
	 * returns true if the all elements of the given array is zero.
	 * <p>配列の全要素が0のときtrueを得る.
	 * 
	 * @param arr  the array to be tested
	 * @return  if the all elements of the given array is null
	 */
	public static boolean isEmpty(short[] arr) {
		for(int i = 0; i < arr.length; i++) {
			if(arr[i] != 0)
				return false;
		}
		return true;
	}
	
	/**
	 * returns true if the all elements of the given array is zero.
	 * <p>配列の全要素が0のときtrueを得る.
	 * 
	 * @param arr  the array to be tested
	 * @return  if the all elements of the given array is null
	 */
	public static boolean isEmpty(int[] arr) {
		for(int i = 0; i < arr.length; i++) {
			if(arr[i] != 0)
				return false;
		}
		return true;
	}
	
	/**
	 * returns true if the all elements of the given array is zero.
	 * <p>配列の全要素が0のときtrueを得る.
	 * 
	 * @param arr  the array to be tested
	 * @return  if the all elements of the given array is null
	 */
	public static boolean isEmpty(long[] arr) {
		for(int i = 0; i < arr.length; i++) {
			if(arr[i] != 0)
				return false;
		}
		return true;
	}
	
	/**
	 * returns true if the all elements of the given array is zero.
	 * <p>配列の全要素が0のときtrueを得る.
	 * 
	 * @param arr  the array to be tested
	 * @return  if the all elements of the given array is null
	 */
	public static boolean isEmpty(float[] arr) {
		for(int i = 0; i < arr.length; i++) {
			if(arr[i] != 0)
				return false;
		}
		return true;
	}
	
	/**
	 * returns true if the all elements of the given array is zero.
	 * <p>配列の全要素が0のときtrueを得る.
	 * 
	 * @param arr  the array to be tested
	 * @return  if the all elements of the given array is null
	 */
	public static boolean isEmpty(double[] arr) {
		for(int i = 0; i < arr.length; i++) {
			if(arr[i] != 0.0)
				return false;
		}
		return true;
	}
	
	/**
	 * resizes the given array to the given size; if size of
	 * the array is not smaller than the given size, this method returns
	 * the given one simply.
	 * <p>配列をリサイズする.配列のサイズがリサイズ後のサイズ以上のときは
	 * 引数の配列をそのまま返す.
	 * 
	 * @param src   the array to be resized
	 * @param size  the size after resized
	 * @return  the resized array
	 */
	public static Object[] resize(Object[] src, int size) {
		if(src.length >= size) {
			return src;
		} else {
			Object[] res = new Object[size];
			
			System.arraycopy(src, 0, res, 0, src.length);
			return res;
		}
	}
	
	/**
	 * resizes the given array to the given size; if size of
	 * the array is not smaller than the given size, this method returns
	 * the given one simply.
	 * <p>配列をリサイズする.配列のサイズがリサイズ後のサイズ以上のときは
	 * 引数の配列をそのまま返す.
	 * 
	 * @param src   the array to be resized
	 * @param size  the size after resized
	 * @return  the resized array
	 */
	public static boolean[] resize(boolean[] src, int size) {
		if(src.length >= size) {
			return src;
		} else {
			boolean[] res = new boolean[size];
			
			System.arraycopy(src, 0, res, 0, src.length);
			return res;
		}
	}
	
	/**
	 * resizes the given array to the given size; if size of
	 * the array is not smaller than the given size, this method returns
	 * the given one simply.
	 * <p>配列をリサイズする.配列のサイズがリサイズ後のサイズ以上のときは
	 * 引数の配列をそのまま返す.
	 * 
	 * @param src   the array to be resized
	 * @param size  the size after resized
	 * @return  the resized array
	 */
	public static byte[] resize(byte[] src, int size) {
		if(src.length >= size) {
			return src;
		} else {
			byte[] res = new byte[size];
			
			System.arraycopy(src, 0, res, 0, src.length);
			return res;
		}
	}
	
	/**
	 * resizes the given array to the given size; if size of
	 * the array is not smaller than the given size, this method returns
	 * the given one simply.
	 * <p>配列をリサイズする.配列のサイズがリサイズ後のサイズ以上のときは
	 * 引数の配列をそのまま返す.
	 * 
	 * @param src   the array to be resized
	 * @param size  the size after resized
	 * @return  the resized array
	 */
	public static char[] resize(char[] src, int size) {
		if(src.length >= size) {
			return src;
		} else {
			char[] res = new char[size];
			
			System.arraycopy(src, 0, res, 0, src.length);
			return res;
		}
	}
	
	/**
	 * resizes the given array to the given size; if size of
	 * the array is not smaller than the given size, this method returns
	 * the given one simply.
	 * <p>配列をリサイズする.配列のサイズがリサイズ後のサイズ以上のときは
	 * 引数の配列をそのまま返す.
	 * 
	 * @param src   the array to be resized
	 * @param size  the size after resized
	 * @return  the resized array
	 */
	public static short[] resize(short[] src, int size) {
		if(src.length >= size) {
			return src;
		} else {
			short[] res = new short[size];
			
			System.arraycopy(src, 0, res, 0, src.length);
			return res;
		}
	}
	
	/**
	 * resizes the given array to the given size; if size of
	 * the array is not smaller than the given size, this method returns
	 * the given one simply.
	 * <p>配列をリサイズする.配列のサイズがリサイズ後のサイズ以上のときは
	 * 引数の配列をそのまま返す.
	 * 
	 * @param src   the array to be resized
	 * @param size  the size after resized
	 * @return  the resized array
	 */
	public static int[] resize(int[] src, int size) {
		if(src.length >= size) {
			return src;
		} else {
			int[] res = new int[size];
			
			System.arraycopy(src, 0, res, 0, src.length);
			return res;
		}
	}
	
	/**
	 * resizes the given array to the given size; if size of
	 * the array is not smaller than the given size, this method returns
	 * the given one simply.
	 * <p>配列をリサイズする.配列のサイズがリサイズ後のサイズ以上のときは
	 * 引数の配列をそのまま返す.
	 * 
	 * @param src   the array to be resized
	 * @param size  the size after resized
	 * @return  the resized array
	 */
	public static long[] resize(long[] src, int size) {
		if(src.length >= size) {
			return src;
		} else {
			long[] res = new long[size];
			
			System.arraycopy(src, 0, res, 0, src.length);
			return res;
		}
	}
	
	/**
	 * resizes the given array to the given size; if size of
	 * the array is not smaller than the given size, this method returns
	 * the given one simply.
	 * <p>配列をリサイズする.配列のサイズがリサイズ後のサイズ以上のときは
	 * 引数の配列をそのまま返す.
	 * 
	 * @param src   the array to be resized
	 * @param size  the size after resized
	 * @return  the resized array
	 */
	public static float[] resize(float[] src, int size) {
		if(src.length >= size) {
			return src;
		} else {
			float[] res = new float[size];
			
			System.arraycopy(src, 0, res, 0, src.length);
			return res;
		}
	}
	
	/**
	 * resizes the given array to the given size; if size of
	 * the array is not smaller than the given size, this method returns
	 * the given one simply.
	 * <p>配列をリサイズする.配列のサイズがリサイズ後のサイズ以上のときは
	 * 引数の配列をそのまま返す.
	 * 
	 * @param src   the array to be resized
	 * @param size  the size after resized
	 * @return  the resized array
	 */
	public static double[] resize(double[] src, int size) {
		if(src.length >= size) {
			return src;
		} else {
			double[] res = new double[size];
			
			System.arraycopy(src, 0, res, 0, src.length);
			return res;
		}
	}
	
	/**
	 * returns the string representation of the given array.
	 * <p>引数の配列の文字列表現を得る.
	 * 
	 * @param a  the array
	 * @return  the string representation of the array
	 */
	public static String toString(Object[] a) {
		StringBuffer buf = new StringBuffer();
		String d2 = "";
		
		for(int i = 0; i < a.length; i++) {
			buf.append(d2);
			buf.append(Objects.toString(a[i]));
			d2 = DELIM;
		}
		return buf.toString();		
	}

	/**
	 * returns the string representation of the given array.
	 * <p>引数の配列の文字列表現を得る.
	 * 
	 * @param a  the array
	 * @return  the string representation of the array
	 */
	public static String toString(boolean[] a) {
		StringBuffer buf = new StringBuffer();
		String d2 = "";
		
		for(int i = 0; i < a.length; i++) {
			buf.append(d2);
			buf.append(Boolean.toString(a[i]));
			d2 = DELIM;
		}
		return buf.toString();		
	}

	/**
	 * returns the string representation of the given array.
	 * <p>引数の配列の文字列表現を得る.
	 * 
	 * @param a  the array
	 * @return  the string representation of the array
	 */
	public static String toString(byte[] a) {
		StringBuffer buf = new StringBuffer();
		String d2 = "";
		
		for(int i = 0; i < a.length; i++) {
			buf.append(d2);
			buf.append(Byte.toString(a[i]));
			d2 = DELIM;
		}
		return buf.toString();		
	}

	/**
	 * returns the string representation of the given array.
	 * <p>引数の配列の文字列表現を得る.
	 * 
	 * @param a  the array
	 * @return  the string representation of the array
	 */
	public static String toString(char[] a) {
		StringBuffer buf = new StringBuffer();
		String d2 = "";
		
		for(int i = 0; i < a.length; i++) {
			buf.append(d2);
			buf.append(Character.toString(a[i]));
			d2 = DELIM;
		}
		return buf.toString();		
	}

	/**
	 * returns the string representation of the given array.
	 * <p>引数の配列の文字列表現を得る.
	 * 
	 * @param a  the array
	 * @return  the string representation of the array
	 */
	public static String toString(short[] a) {
		StringBuffer buf = new StringBuffer();
		String d2 = "";
		
		for(int i = 0; i < a.length; i++) {
			buf.append(d2);
			buf.append(Short.toString(a[i]));
			d2 = DELIM;
		}
		return buf.toString();		
	}

	/**
	 * returns the string representation of the given array.
	 * <p>引数の配列の文字列表現を得る.
	 * 
	 * @param a  the array
	 * @return  the string representation of the array
	 */
	public static String toString(int[] a) {
		StringBuffer buf = new StringBuffer();
		String d2 = "";
		
		for(int i = 0; i < a.length; i++) {
			buf.append(d2);
			buf.append(Integer.toString(a[i]));
			d2 = DELIM;
		}
		return buf.toString();		
	}

	/**
	 * returns the string representation of the given array.
	 * <p>引数の配列の文字列表現を得る.
	 * 
	 * @param a  the array
	 * @return  the string representation of the array
	 */
	public static String toString(long[] a) {
		StringBuffer buf = new StringBuffer();
		String d2 = "";
		
		for(int i = 0; i < a.length; i++) {
			buf.append(d2);
			buf.append(Long.toString(a[i]));
			d2 = DELIM;
		}
		return buf.toString();		
	}

	/**
	 * returns the string representation of the given array.
	 * <p>引数の配列の文字列表現を得る.
	 * 
	 * @param a  the array
	 * @return  the string representation of the array
	 */
	public static String toString(float[] a) {
		StringBuffer buf = new StringBuffer();
		String d2 = "";
		
		for(int i = 0; i < a.length; i++) {
			buf.append(d2);
			buf.append(Float.toString(a[i]));
			d2 = DELIM;
		}
		return buf.toString();		
	}

	/**
	 * returns the string representation of the given array.
	 * <p>引数の配列の文字列表現を得る.
	 * 
	 * @param a  the array
	 * @return  the string representation of the array
	 */
	public static String toString(double[] a) {
		StringBuffer buf = new StringBuffer();
		String d2 = "";
		
		for(int i = 0; i < a.length; i++) {
			buf.append(d2);
			buf.append(Double.toString(a[i]));
			d2 = DELIM;
		}
		return buf.toString();		
	}
	
	/**
	 * returns true if the given array contains an null pointer.
	 * <p>配列の要素にヌルが含まれるときtrueを得る.
	 * 
	 * @param a  the array
	 * @return  true if the given array contains an null pointer
	 */
	public static boolean containsNull(Object[] a) {
		for(int i = 0; i < a.length; i++) {
			if(a[i] == null) {
				return true;
			}
		}
		return false;
	}
	
	//
	private static<E> int countLength(E[]... es) {
		int res = 0;
		
		for(E[] a : es) {
			res += a.length;
		}
		return res;
	}
	
	/**
	 * Joins the given arrays.
	 * <p>与えられた配列を結合します。
	 * 
	 * @param es arrays to be joined
	 * @return new joined array
	 */
	@SuppressWarnings("unchecked")
	public static<E> E[] join(E[]... es) {
		int leng = countLength(es);
		Object[] res = new Object[leng];
		int cnt = 0;
		
		for(E[] a : es) {
			System.arraycopy(a, 0, res, cnt, a.length);
			cnt += a.length;
		}
		return (E[])res;
	}
	
	/**
	 * Adds the given elements to the end of the given array.
	 * <p>要素を配列の末尾に追加します。
	 * 
	 * @param src an array to add
	 * @param es elements to be added
	 * @return new added array
	 */
	@SuppressWarnings("unchecked")
	public static<E> E[] add(E[] src, E... es) {
		return join(src, es);
	}
	
	//
	private static<E> int countLength(int[]... es) {
		int res = 0;
		
		for(int[] a : es) {
			res += a.length;
		}
		return res;
	}
	
	/**
	 * Joins the given arrays.
	 * <p>与えられた配列を結合します。
	 * 
	 * @param es arrays to be joined
	 * @return new joined array
	 */
	public static int[] join(int[]... es) {
		int leng = countLength(es);
		int[] res = new int[leng];
		int cnt = 0;
		
		for(int[] a : es) {
			System.arraycopy(a, 0, res, cnt, a.length);
			cnt += a.length;
		}
		return res;
	}
	
	/**
	 * Adds the given elements to the end of the given array.
	 * <p>要素を配列の末尾に追加します。
	 * 
	 * @param src an array to add
	 * @param es elements to be added
	 * @return new added array
	 */
	public static int[] add(int[] src, int... es) {
		return join(src, es);
	}
	
}
