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
package net.morilib.math.combinations;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/08
 */
public final class CombinationUtils {

	//
	private CombinationUtils() {}

	/**
	 * 
	 * @param l
	 * @param m
	 * @return
	 */
	public static int[] initCombination(int l, int m) {
		int[] r = new int[m];

		if(l < m) {
			throw new IllegalArgumentException();
		}
		for(int i = 0; i < m; i++)  r[i] = i;
		return r;
	}

	/**
	 * 
	 * @param val
	 * @return
	 */
	public static int[] nextCombination(int l, int[] val) {
		int i = val.length - 1;

		for(int m = 1; i >= 0; i--, m++) {
			if(val[i] < l - m) {
				val[i]++;
				for(int j = i + 1; j < val.length; j++) {
					val[j] = val[i] + j - i;
				}
				return val;
			}
		}
		return null;
	}

}
