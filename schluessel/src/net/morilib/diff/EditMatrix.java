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
package net.morilib.diff;

import java.util.Arrays;

import net.morilib.lang.EqualPredicate;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/03/10
 */
class EditMatrix<T> {

	//
	@SuppressWarnings("rawtypes")
	EditScript[] scripts;
	int[] distances;
	private int sizeA, sizeB;

	//
	EditMatrix(int a, int b) {
		sizeA = a + 1;
		sizeB = b + 1;
		scripts = new EditScript[sizeA * sizeB];
		distances = new int[sizeA * sizeB];

		Arrays.fill(distances, -1);
		scripts  [0] = new InitialScript<T>();
		distances[0] = 0;
	}

	//
	@SuppressWarnings("unchecked")
	EditScript<T> getScripts(int i, int j) {
		if(i < 0 || i >= sizeA) {
			throw new IndexOutOfBoundsException();
		} else if(j < 0 || j >= sizeB) {
			throw new IndexOutOfBoundsException();
		}
		return scripts[i * sizeB + j];
	}

	//
	void setScripts(int i, int j, EditScript<T> s) {
		if(i < 0 || i >= sizeA) {
			throw new IndexOutOfBoundsException();
		} else if(j < 0 || j >= sizeB) {
			throw new IndexOutOfBoundsException();
		}
		scripts[i * sizeB + j] = s;
	}

	//
	int getDistance(int i, int j) {
		if(i < 0 || i >= sizeA) {
			throw new IndexOutOfBoundsException();
		} else if(j < 0 || j >= sizeB) {
			throw new IndexOutOfBoundsException();
		}
		return distances[i * sizeB + j];
	}

	//
	void setDistance(int i, int j, int d) {
		if(i < 0 || i >= sizeA) {
			throw new IndexOutOfBoundsException();
		} else if(j < 0 || j >= sizeB) {
			throw new IndexOutOfBoundsException();
		}
		distances[i * sizeB + j] = d;
	}

	//
	boolean isKnown(int i, int j) {
		return getDistance(i, j) >= 0;
	}

	//
	boolean canApplyRule1(int m, int i, int j) {
		return (i >= 0 && i < sizeA &&
				j >  0 && j < sizeB &&
				isKnown(i, j - 1) &&
				getDistance(i, j - 1) < m &&
				getScripts(i, j - 1) != null &&
				!isKnown(i, j));
	}

	//
	boolean canApplyRule2(int m, int i, int j) {
		return (i >  0 && i < sizeA &&
				j >= 0 && j < sizeB &&
				isKnown(i - 1, j) &&
				getDistance(i - 1, j) < m &&
				getScripts(i - 1, j) != null &&
				!isKnown(i, j));
	}

	//
	boolean canApplyRule3(int m, int i, int j, T a, T b,
			EqualPredicate eq) {
		return (i >  0 && i < sizeA &&
				j >  0 && j < sizeB &&
				isKnown(i - 1, j - 1) &&
				getDistance(i - 1, j - 1) < m &&
				getScripts(i - 1, j - 1) != null &&
				eq.isEqual(a, b));
	}

}
