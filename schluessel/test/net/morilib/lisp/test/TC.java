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
package net.morilib.lisp.test;

import java.util.Arrays;

import junit.framework.TestCase;

public abstract class TC extends TestCase {

	/**
	 * 
	 * @param o1
	 * @param o2
	 */
	public static void eq(Object o1, Object o2) {
		assertEquals(o2, o1);
	}
	
	/**
	 * 
	 * @param o1
	 * @param o2
	 */
	public static void eq(boolean o1, boolean o2) {
		assertEquals(o2, o1);
	}
	
	/**
	 * 
	 * @param o1
	 * @param o2
	 */
	public static void eq(int o1, int o2) {
		assertEquals(o2, o1);
	}
	
	/**
	 * 
	 * @param o1
	 * @param o2
	 */
	public static void eq(long o1, long o2) {
		assertEquals(o2, o1);
	}
	
	/**
	 * 
	 * @param o1
	 * @param o2
	 */
	public static void eq(double o1, double o2) {
		assertTrue(
				("<" + Double.toString(o2) + "> but <" +
						Double.toString(o1) + ">"),
				o1 == o2);
	}
	
	/**
	 * 
	 * @param o1
	 */
	public static void nil(Object o1) {
		assertNull(o1);
	}
	
	/**
	 * 
	 * @param o1
	 */
	public static void ok(boolean o1) {
		assertTrue(o1);
	}
	
	/**
	 * 
	 * @param o1
	 */
	public static void ng(boolean o1) {
		assertFalse(o1);
	}
	
	/**
	 * 
	 * @param o1
	 * @param o2
	 */
	public static void eq(Object[] o1, Object[] o2) {
		assertTrue(
				Arrays.toString(o2) + " but " + Arrays.toString(o1),
				Arrays.equals(o1, o2));
	}
	
	/**
	 * 
	 * @param o1
	 * @param o2
	 */
	public static void eq(int[] o1, int[] o2) {
		assertTrue(
				Arrays.toString(o2) + " but " + Arrays.toString(o1),
				Arrays.equals(o1, o2));
	}
	
	/**
	 * 
	 * @param o1
	 * @param o2
	 */
	public static void eq(byte[] o1, byte[] o2) {
		assertTrue(
				Arrays.toString(o2) + " but " + Arrays.toString(o1),
				Arrays.equals(o1, o2));
	}
	
	/**
	 * 
	 * @param o1
	 * @param o2
	 */
	public static void eq(short[] o1, short[] o2) {
		assertTrue(
				Arrays.toString(o2) + " but " + Arrays.toString(o1),
				Arrays.equals(o1, o2));
	}
	
	/**
	 * 
	 * @param o1
	 * @param o2
	 */
	public static void eq(long[] o1, long[] o2) {
		assertTrue(
				Arrays.toString(o2) + " but " + Arrays.toString(o1),
				Arrays.equals(o1, o2));
	}
	
	/**
	 * 
	 * @param o1
	 * @param o2
	 */
	public static void eq(float[] o1, float[] o2) {
		assertTrue(
				Arrays.toString(o2) + " but " + Arrays.toString(o1),
				Arrays.equals(o1, o2));
	}
	
	/**
	 * 
	 * @param o1
	 * @param o2
	 */
	public static void eq(double[] o1, double[] o2) {
		assertTrue(
				Arrays.toString(o2) + " but " + Arrays.toString(o1),
				Arrays.equals(o1, o2));
	}
	
	/**
	 * 
	 * @param o1
	 * @param o2
	 */
	public static void eq(boolean[] o1, boolean[] o2) {
		assertTrue(
				Arrays.toString(o2) + " but " + Arrays.toString(o1),
				Arrays.equals(o1, o2));
	}
	
	/**
	 * 
	 * @param o1
	 * @param o2
	 */
	public static void eq(char[] o1, char[] o2) {
		assertTrue(
				Arrays.toString(o2) + " but " + Arrays.toString(o1),
				Arrays.equals(o1, o2));
	}
	
	/**
	 * 
	 * @param o1
	 * @param o2
	 */
	public static void ne(boolean o1, boolean o2) {
		assertTrue(
				("<" + Boolean.toString(o2) + "> but <" +
						Boolean.toString(o1) + ">"),
				o1 != o2);
	}
	
	/**
	 * 
	 * @param o1
	 * @param o2
	 */
	public static void ne(byte o1, byte o2) {
		assertTrue(
				("<" + Byte.toString(o2) + "> but <" +
						Byte.toString(o1) + ">"),
				o1 != o2);
	}
	
	/**
	 * 
	 * @param o1
	 * @param o2
	 */
	public static void ne(char o1, char o2) {
		assertTrue(
				("<" + Character.toString(o2) + "> but <" +
						Character.toString(o1) + ">"),
				o1 != o2);
	}
	
	/**
	 * 
	 * @param o1
	 * @param o2
	 */
	public static void ne(short o1, short o2) {
		assertTrue(
				("<" + Short.toString(o2) + "> but <" +
						Short.toString(o1) + ">"),
				o1 != o2);
	}
	
	/**
	 * 
	 * @param o1
	 * @param o2
	 */
	public static void ne(int o1, int o2) {
		assertTrue(
				("<" + Integer.toString(o2) + "> but <" +
						Integer.toString(o1) + ">"),
				o1 != o2);
	}
	
	/**
	 * 
	 * @param o1
	 * @param o2
	 */
	public static void ne(long o1, long o2) {
		assertTrue(
				("<" + Long.toString(o2) + "> but <" +
						Long.toString(o1) + ">"),
				o1 != o2);
	}
	
	/**
	 * 
	 * @param o1
	 * @param o2
	 */
	public static void ne(float o1, float o2) {
		assertTrue(
				("<" + Float.toString(o2) + "> but <" +
						Float.toString(o1) + ">"),
				o1 != o2);
	}
	
	/**
	 * 
	 * @param o1
	 * @param o2
	 */
	public static void ne(double o1, double o2) {
		assertTrue(
				("<" + Double.toString(o2) + "> but <" +
						Double.toString(o1) + ">"),
				o1 != o2);
	}
	
	private static String str(Object o1) {
		return ((o1 == null) ? "null" : o1.toString());
	}
	
	/**
	 * 
	 * @param o1
	 * @param o2
	 */
	public static void ne(Object o1, Object o2) {
		assertTrue(
				("<" + str(o2) + "> but <" + str(o1) + ">"),
				!o1.equals(o2));
	}
	
	/**
	 * 
	 * @param s
	 */
	public void out(Object s) {
		System.out.println((s == null) ? "null" : s.toString());
	}
	
	/**
	 * 
	 * @param s
	 */
	public void err(Object s) {
		System.err.println((s == null) ? "null" : s.toString());
	}
	
}
