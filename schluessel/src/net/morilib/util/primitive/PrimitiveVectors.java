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
 * @author MORIGUCHI, Yuichiro 2012/01/10
 */
public final class PrimitiveVectors {

	//
	private PrimitiveVectors() {}

	/**
	 * 
	 * @param vec
	 * @return
	 */
	public static boolean isSorted(CharacterVector vec) {
		for(int i = 1; i < vec.size(); i++) {
			if(vec.get(i - 1) > vec.get(i)) {
				return false;
			}
		}
		return true;
	}

	/**
	 * 
	 * @param vec
	 * @param cmp
	 * @return
	 */
	public static boolean isSorted(CharacterVector vec,
			CharacterComparator cmp) {
		for(int i = 1; i < vec.size(); i++) {
			if(cmp.compare(vec.getChar(i - 1), vec.getChar(i)) > 0) {
				return false;
			}
		}
		return true;
	}

	/**
	 * 
	 * @param vec
	 * @param mvec
	 */
	public static void merge(CharacterVector vec,
			CharacterVector mvec) {
		merge(vec, mvec, PrimitiveComparators.NATURAL_CHAR);
	}

	/**
	 * 
	 * @param vec
	 * @param mvec
	 * @param cmp
	 */
	public static void merge(CharacterVector vec, CharacterVector mvec,
			CharacterComparator cmp) {
		int i = 0, j = 0, c;

		while(j < vec.size()) {
			if(i >= vec.size()) {
				vec.add(mvec.get(j++));
			} else if((c = cmp.compare(
					vec.get(i), mvec.get(j))) < 0) {
				i++;
			} else if(c == 0) {
				i++;  j++;
			} else {
				vec.add(i, mvec.get(j));
				i += 2;  j++;
			}
		}
	}

}
