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

import java.math.BigInteger;

/**
 * An utility class for arithmetics of int value.
 * <p>intの算術のためのユーティリティクラスである。
 * 
 * @author MORIGUCHI, Yuichiro 2010/04/18
 */
public final class IntMath {
	
	private IntMath() {
		// do noting
	}
	
	/**
	 * returns 1 when the given value is positive,
	 * returns -1 when the value is negative,
	 * otherwise returns 0.
	 * <p>与えられた値が正のとき1を、負のとき-1を、
	 * それ以外のときは0を得る。
	 * 
	 * @param value value to be tested
	 */
	public static int signum(int value) {
		return (value > 0) ? 1 : (value < 0) ? -1 : 0;
	}
	
	/**
	 * calculates the G.C.D. of the given values.
	 * <p>与えられた数の最大公約数を得る。
	 * 
	 * @param v1 value to be calculated 
	 * @param v2 another value to be calculated
	 * @return G.C.D. of the given values
	 */
	public static int gcd(int v1, int v2) {
		int x = (v1 < 0) ? -v1 : v1;
		int y = (v2 < 0) ? -v2 : v2;
		
		if(x == 0 && y == 0) {
			return 0;
		} else {
			while(y != 0) {
				int t = x % y;
				
				x = y;
				y = t;
			}
			return x;
		}
	}
	
	/**
	 * subtracts b from a when b is less than a,
	 * returns 0 when b is more than a.
	 * <p>bがaより小さいときはb-aを、
	 * bがaより大きいときは0を得る。
	 * 
	 * @param a value to be subtracted
	 * @param b value to subtract
	 */
	public static int natsub(int a, int b) {
		return (a - b) > 0 ? a - b : 0;
	}
	
	
	public static int tomult(int d, int m) {
		return (d / m) * m + signum(d % m) * m;
	}
	
	
	public static int max(int a, int b) {
		return (a < b) ? b : a;
	}
	
	
	public static int min(int a, int b) {
		return (a < b) ? a : b;
	}
	
	
	public static int pow(int x, int y) {
		return BigInteger.valueOf(x).pow(y).intValue();
	}
	
	
	public static int compareTo(int x, int y) {
		return (x > y) ? 1 : (x < y) ? -1 : 0;
	}
	
	
	public static int compareTo(long x, long y) {
		return (x > y) ? 1 : (x < y) ? -1 : 0;
	}
	
}
