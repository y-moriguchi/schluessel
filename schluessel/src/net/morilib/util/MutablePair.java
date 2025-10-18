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
package net.morilib.util;

import net.morilib.lang.Hashes;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/04/16
 */
public class MutablePair<A, B> implements Pair<A, B> {

	/**
	 * 
	 */
	protected A valueA;

	/**
	 * 
	 */
	protected B valueB;

	/**
	 * 
	 */
	public MutablePair() { }

	/**
	 * 
	 * @param a
	 * @param b
	 */
	public MutablePair(A a, B b) {
		valueA = a;
		valueB = b;
	}

	/**
	 * gets the first value of this pair.
	 * <p>対の第1の値を得る.
	 */
	public A getA() {
		return valueA;
	}

	/**
	 * gets the second value of this pair.
	 * <p>対の第2の値を得る.
	 */
	public B getB() {
		return valueB;
	}

	/**
	 * 
	 * @param a
	 */
	public void setA(A a) {
		this.valueA = a;
	}

	/**
	 * 
	 * @param b
	 */
	public void setB(B b) {
		this.valueB = b;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		if(o == null) {
			return false;
		} else if(o instanceof MutablePair<?, ?>) {
			MutablePair<?, ?> o2 = (MutablePair<?, ?>)o;

			return (Objects.equals(valueA, o2.valueA) &&
					Objects.equals(valueB, o2.valueB));
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int res = Hashes.INIT;

		res = Hashes.A * res + Hashes.hashCode(valueA);
		res = Hashes.A * res + Hashes.hashCode(valueB);
		return res;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return "(" + valueA + "," + valueB + ")";
	}

}
