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

import net.morilib.lisp.test.TC;

/**
 * 
 * 
 * @author MORIGUCHI, Yuichiro 2005/10/29
 */
public class Arrays2Test extends TC {

	//
	static final Object[]  AO1 = new Object[]  { "a", "b", "c" };
	static final boolean[] AB1 = new boolean[] { true, false, true };
	static final byte[]    AY1 = new byte[]    { 1, 2, 3 };
	static final char[]    AC1 = new char[]    { 'a', 'b', 'c' };
	static final int[]     AI1 = new int[]     { 1, 2, 3 };
	static final long[]    AL1 = new long[]    { 1, 2, 3 };
	static final short[]   AS1 = new short[]   { 1, 2, 3 };
	static final float[]   AF1 = new float[]   { 1, 2, 3 };
	static final double[]  AD1 = new double[]  { 1, 2, 3 };
	static final String[]  AO2 = new String[]  { "a", "b", "c" };
	
	/**
	 * 
	 *
	 */
	public void testEqualsOO() {
		Object[] r1 = new Object[] { new String("a"), "b" ,"c" };
		Object[] r2 = new Object[] { new String("a"), "d" ,"c" };
		Object[] r3 = new Object[] { new String("a"), "b" };
		
		ok(Arrays2.equals(AO1, r1));
		ng(Arrays2.equals(AO1, r2));
		ng(Arrays2.equals(AO1, r3));
	}
	
	/**
	 * 
	 *
	 */
	public void testEqualsII() {
		int[] r1 = new int[] { 1, 2, 3 };
		int[] r2 = new int[] { 1, 3, 2 };
		int[] r3 = new int[] { 1, 2 };
		
		ok(Arrays2.equals(AI1, r1));
		ng(Arrays2.equals(AI1, r2));
		ng(Arrays2.equals(AI1, r3));
	}
	
	/**
	 * 
	 *
	 */
	public void testEqualsLL() {
		long[] r1 = new long[] { 1, 2, 3 };
		long[] r2 = new long[] { 1, 3, 2 };
		long[] r3 = new long[] { 1, 2 };
		
		ok(Arrays2.equals(AL1, r1));
		ng(Arrays2.equals(AL1, r2));
		ng(Arrays2.equals(AL1, r3));
	}
	
	/**
	 * 
	 *
	 */
	public void testEqualsYY() {
		byte[] r1 = new byte[] { 1, 2, 3 };
		byte[] r2 = new byte[] { 1, 3, 2 };
		byte[] r3 = new byte[] { 1, 2 };
		
		ok(Arrays2.equals(AY1, r1));
		ng(Arrays2.equals(AY1, r2));
		ng(Arrays2.equals(AY1, r3));
	}
	
	/**
	 * 
	 *
	 */
	public void testEqualsCC() {
		char[] r1 = new char[] { 'a', 'b', 'c' };
		char[] r2 = new char[] { 'a', 'd', 'c' };
		char[] r3 = new char[] { 'a', 'b' };
		
		ok(Arrays2.equals(AC1, r1));
		ng(Arrays2.equals(AC1, r2));
		ng(Arrays2.equals(AC1, r3));
	}
	
	/**
	 * 
	 *
	 */
	public void testEqualsSS() {
		short[] r1 = new short[] { 1, 2, 3 };
		short[] r2 = new short[] { 1, 3, 2 };
		short[] r3 = new short[] { 1, 2 };
		
		ok(Arrays2.equals(AS1, r1));
		ng(Arrays2.equals(AS1, r2));
		ng(Arrays2.equals(AS1, r3));
	}
	
	/**
	 * 
	 *
	 */
	public void testEqualsFF() {
		float[] r1 = new float[] { 1, 2, 3 };
		float[] r2 = new float[] { 1, 3, 2 };
		float[] r3 = new float[] { 1, 2 };
		
		ok(Arrays2.equals(AF1, r1));
		ng(Arrays2.equals(AF1, r2));
		ng(Arrays2.equals(AF1, r3));
	}
	
	/**
	 * 
	 *
	 */
	public void testEqualsDD() {
		double[] r1 = new double[] { 1, 2, 3 };
		double[] r2 = new double[] { 1, 3, 2 };
		double[] r3 = new double[] { 1, 2 };
		
		ok(Arrays2.equals(AD1, r1));
		ng(Arrays2.equals(AD1, r2));
		ng(Arrays2.equals(AD1, r3));
	}
	
	/**
	 * 
	 *
	 */
	public void testEqualsBB() {
		boolean[] r1 = new boolean[] { true, false, true };
		boolean[] r2 = new boolean[] { false, false, true };
		boolean[] r3 = new boolean[] { true, false };
		
		ok(Arrays2.equals(AB1, r1));
		ng(Arrays2.equals(AB1, r2));
		ng(Arrays2.equals(AB1, r3));
	}
	
	/**
	 * 
	 *
	 */
	public void testCopyOiiO() {
		String[] r = new String[2];
		eq(Arrays2.copy(AO2, 1, 2, r), new String[] { "b", "c" });
	}
	
	/**
	 * 
	 *
	 */
	public void testCopyOii() {
		eq(Arrays2.copy(AO2, 1, 2), new String[] { "b", "c" });
	}
	
	/**
	 * 
	 *
	 */
	public void testCopyOO() {
		String[] r = new String[3];
		eq(Arrays2.copy(AO2, r), new String[] { "a", "b", "c" });
	}
	
	/**
	 * 
	 *
	 */
	public void testCopyO() {
		eq(Arrays2.copy(AO2), new String[] { "a", "b", "c" });
	}
	
	/**
	 * 
	 *
	 */
	public void testCopyBiiB() {
		boolean[] r = new boolean[2];
		eq(Arrays2.copy(AB1, 1, 2, r), new boolean[] { false, true });
	}
	
	/**
	 * 
	 *
	 */
	public void testCopyBii() {
		eq(Arrays2.copy(AB1, 1, 2), new boolean[] { false, true });
	}
	
	/**
	 * 
	 *
	 */
	public void testCopyBB() {
		boolean[] r = new boolean[3];
		eq(Arrays2.copy(AB1, r), new boolean[] { true, false, true });
	}
	
	/**
	 * 
	 *
	 */
	public void testCopyB() {
		eq(Arrays2.copy(AB1), new boolean[] { true, false, true });
	}
	
	/**
	 * 
	 *
	 */
	public void testCopyYiiY() {
		byte[] r = new byte[2];
		eq(Arrays2.copy(AY1, 1, 2, r), new byte[] { 2, 3 });
	}
	
	/**
	 * 
	 *
	 */
	public void testCopyYii() {
		eq(Arrays2.copy(AY1, 1, 2), new byte[] { 2, 3 });
	}
	
	/**
	 * 
	 *
	 */
	public void testCopyYY() {
		byte[] r = new byte[3];
		eq(Arrays2.copy(AY1, r), new byte[] { 1, 2, 3 });
	}
	
	/**
	 * 
	 *
	 */
	public void testCopyY() {
		eq(Arrays2.copy(AY1), new byte[] { 1, 2, 3 });
	}
	
	/**
	 * 
	 *
	 */
	public void testCopyCiiC() {
		char[] r = new char[2];
		eq(Arrays2.copy(AC1, 1, 2, r), new char[] { 'b', 'c' });
	}
	
	/**
	 * 
	 *
	 */
	public void testCopyCii() {
		eq(Arrays2.copy(AC1, 1, 2), new char[] { 'b', 'c' });
	}
	
	/**
	 * 
	 *
	 */
	public void testCopyCC() {
		char[] r = new char[3];
		eq(Arrays2.copy(AC1, r), new char[] { 'a', 'b', 'c' });
	}
	
	/**
	 * 
	 *
	 */
	public void testCopyC() {
		eq(Arrays2.copy(AC1), new char[] { 'a', 'b', 'c' });
	}
	
	/**
	 * 
	 *
	 */
	public void testCopySiiS() {
		short[] r = new short[2];
		eq(Arrays2.copy(AS1, 1, 2, r), new short[] { 2, 3 });
	}
	
	/**
	 * 
	 *
	 */
	public void testCopySii() {
		eq(Arrays2.copy(AS1, 1, 2), new short[] { 2, 3 });
	}
	
	/**
	 * 
	 *
	 */
	public void testCopySS() {
		short[] r = new short[3];
		eq(Arrays2.copy(AS1, r), new short[] { 1, 2, 3 });
	}
	
	/**
	 * 
	 *
	 */
	public void testCopyS() {
		eq(Arrays2.copy(AS1), new short[] { 1, 2, 3 });
	}
	
	/**
	 * 
	 *
	 */
	public void testCopyIiiI() {
		int[] r = new int[2];
		eq(Arrays2.copy(AI1, 1, 2, r), new int[] { 2, 3 });
	}
	
	/**
	 * 
	 *
	 */
	public void testCopyIii() {
		eq(Arrays2.copy(AI1, 1, 2), new int[] { 2, 3 });
	}
	
	/**
	 * 
	 *
	 */
	public void testCopyII() {
		int[] r = new int[3];
		eq(Arrays2.copy(AI1, r), new int[] { 1, 2, 3 });
	}
	
	/**
	 * 
	 *
	 */
	public void testCopyI() {
		eq(Arrays2.copy(AI1), new int[] { 1, 2, 3 });
	}
	
	/**
	 * 
	 *
	 */
	public void testCopyLiiL() {
		long[] r = new long[2];
		eq(Arrays2.copy(AL1, 1, 2, r), new long[] { 2, 3 });
	}
	
	/**
	 * 
	 *
	 */
	public void testCopyLii() {
		eq(Arrays2.copy(AL1, 1, 2), new long[] { 2, 3 });
	}
	
	/**
	 * 
	 *
	 */
	public void testCopyLL() {
		long[] r = new long[3];
		eq(Arrays2.copy(AL1, r), new long[] { 1, 2, 3 });
	}
	
	/**
	 * 
	 *
	 */
	public void testCopyL() {
		eq(Arrays2.copy(AL1), new long[] { 1, 2, 3 });
	}
	
	/**
	 * 
	 *
	 */
	public void testCopyFiiF() {
		float[] r = new float[2];
		eq(Arrays2.copy(AF1, 1, 2, r), new float[] { 2, 3 });
	}
	
	/**
	 * 
	 *
	 */
	public void testCopyFii() {
		eq(Arrays2.copy(AF1, 1, 2), new float[] { 2, 3 });
	}
	
	/**
	 * 
	 *
	 */
	public void testCopyFF() {
		float[] r = new float[3];
		eq(Arrays2.copy(AF1, r), new float[] { 1, 2, 3 });
	}
	
	/**
	 * 
	 *
	 */
	public void testCopyF() {
		eq(Arrays2.copy(AF1), new float[] { 1, 2, 3 });
	}
	
	/**
	 * 
	 *
	 */
	public void testCopyDiiD() {
		double[] r = new double[2];
		eq(Arrays2.copy(AD1, 1, 2, r), new double[] { 2, 3 });
	}
	
	/**
	 * 
	 *
	 */
	public void testCopyDii() {
		eq(Arrays2.copy(AD1, 1, 2), new double[] { 2, 3 });
	}
	
	/**
	 * 
	 *
	 */
	public void testCopyDD() {
		double[] r = new double[3];
		eq(Arrays2.copy(AD1, r), new double[] { 1, 2, 3 });
	}
	
	/**
	 * 
	 *
	 */
	public void testCopyD() {
		eq(Arrays2.copy(AD1), new double[] { 1, 2, 3 });
	}
	
	/**
	 * 
	 *
	 */
	public void testIsAllNull() {
		ok(Arrays2.isAllNull(new Object[] { null, null }));
		ng(Arrays2.isAllNull(new Object[] { null, "a" }));
	}
	
	/**
	 * 
	 *
	 */
	public void testIsAllFalse() {
		ok(Arrays2.isAllFalse(new boolean[] { false, false }));
		ng(Arrays2.isAllFalse(new boolean[] { false, true }));
		ng(Arrays2.isAllFalse(new boolean[] { true, true }));
	}
	
	/**
	 * 
	 *
	 */
	public void testIsAllTrue() {
		ng(Arrays2.isAllTrue(new boolean[] { false, false }));
		ng(Arrays2.isAllTrue(new boolean[] { false, true }));
		ok(Arrays2.isAllTrue(new boolean[] { true, true }));
	}
	
	/**
	 * 
	 *
	 */
	public void testIsEmptyY() {
		ok(Arrays2.isEmpty(new byte[] { 0, 0 }));
		ng(Arrays2.isEmpty(new byte[] { 0, 1 }));
	}
	
	/**
	 * 
	 *
	 */
	public void testIsEmptyS() {
		ok(Arrays2.isEmpty(new short[] { 0, 0 }));
		ng(Arrays2.isEmpty(new short[] { 0, 1 }));
	}
	
	/**
	 * 
	 *
	 */
	public void testIsEmptyI() {
		ok(Arrays2.isEmpty(new int[] { 0, 0 }));
		ng(Arrays2.isEmpty(new int[] { 0, 1 }));
	}
	
	/**
	 * 
	 *
	 */
	public void testIsEmptyL() {
		ok(Arrays2.isEmpty(new long[] { 0, 0 }));
		ng(Arrays2.isEmpty(new long[] { 0, 1 }));
	}
	
	/**
	 * 
	 *
	 */
	public void testIsEmptyF() {
		ok(Arrays2.isEmpty(new float[] { 0, 0 }));
		ng(Arrays2.isEmpty(new float[] { 0, 1 }));
	}
	
	/**
	 * 
	 *
	 */
	public void testIsEmptyD() {
		ok(Arrays2.isEmpty(new double[] { 0, 0 }));
		ng(Arrays2.isEmpty(new double[] { 0, 1 }));
	}
	
	/**
	 * 
	 *
	 */
	public void testResizeOi() {
		eq(Arrays2.resize(AO1, 2), AO1);
		eq(Arrays2.resize(AO1, 3), AO1);
		eq(Arrays2.resize(AO1, 4), new Object[] { "a", "b", "c", null });
	}
	
	/**
	 * 
	 *
	 */
	public void testResizeBi() {
		eq(Arrays2.resize(AB1, 2), AB1);
		eq(Arrays2.resize(AB1, 3), AB1);
		eq(Arrays2.resize(AB1, 4),
				new boolean[] { true, false, true, false });
	}
	
	/**
	 * 
	 *
	 */
	public void testResizeYi() {
		eq(Arrays2.resize(AY1, 2), AY1);
		eq(Arrays2.resize(AY1, 3), AY1);
		eq(Arrays2.resize(AY1, 4), new byte[] { 1, 2, 3, 0 });
	}
	
	/**
	 * 
	 *
	 */
	public void testResizeCi() {
		eq(Arrays2.resize(AC1, 2), AC1);
		eq(Arrays2.resize(AC1, 3), AC1);
		eq(Arrays2.resize(AC1, 4), new char[] { 'a', 'b', 'c', 0 });
	}
	
	/**
	 * 
	 *
	 */
	public void testResizeSi() {
		eq(Arrays2.resize(AS1, 2), AS1);
		eq(Arrays2.resize(AS1, 3), AS1);
		eq(Arrays2.resize(AS1, 4), new short[] { 1, 2, 3, 0 });
	}
	
	/**
	 * 
	 *
	 */
	public void testResizeIi() {
		eq(Arrays2.resize(AI1, 2), AI1);
		eq(Arrays2.resize(AI1, 3), AI1);
		eq(Arrays2.resize(AI1, 4), new int[] { 1, 2, 3, 0 });
	}
	
	/**
	 * 
	 *
	 */
	public void testResizeLi() {
		eq(Arrays2.resize(AL1, 2), AL1);
		eq(Arrays2.resize(AL1, 3), AL1);
		eq(Arrays2.resize(AL1, 4), new long[] { 1, 2, 3, 0 });
	}
	
	/**
	 * 
	 *
	 */
	public void testResizeFi() {
		eq(Arrays2.resize(AF1, 2), AF1);
		eq(Arrays2.resize(AF1, 3), AF1);
		eq(Arrays2.resize(AF1, 4), new float[] { 1, 2, 3, 0 });
	}
	
	/**
	 * 
	 *
	 */
	public void testResizeDi() {
		eq(Arrays2.resize(AD1, 2), AD1);
		eq(Arrays2.resize(AD1, 3), AD1);
		eq(Arrays2.resize(AD1, 4), new double[] { 1, 2, 3, 0 });
	}

}
